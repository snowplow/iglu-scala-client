/*
 * Copyright (c) 2018-2023 Snowplow Analytics Ltd. All rights reserved.
 *
 * This program is licensed to you under the Apache License Version 2.0,
 * and you may not use this file except in compliance with the Apache License Version 2.0.
 * You may obtain a copy of the Apache License Version 2.0 at http://www.apache.org/licenses/LICENSE-2.0.
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the Apache License Version 2.0 is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the Apache License Version 2.0 for the specific language governing permissions and limitations there under.
 */
package com.snowplowanalytics.iglu.client
package resolver

import scala.io.Source

// Cats
import cats.Applicative
import cats.data.State
import cats.effect.{Clock, IO}
import cats.syntax.either._

import scala.concurrent.duration.{DurationInt, FiniteDuration}

// Circe
import io.circe.Json
import io.circe.parser._

// LRU Map
import com.snowplowanalytics.iglu.core.circe.implicits._
import com.snowplowanalytics.iglu.core.{
  SchemaKey,
  SchemaList,
  SchemaMap,
  SchemaVer,
  SelfDescribingSchema
}
import com.snowplowanalytics.lrumap.LruMap

// This project
import com.snowplowanalytics.iglu.client.resolver.registries.{
  JavaNetRegistryLookup,
  Registry,
  RegistryError,
  RegistryLookup
}

object ResolverSpecHelpers {

  /**
   * State of the "resolver's world" to test every aspect of the resolution in a pure FP way
   * @param requestsCounter
   *   map of "registry name -> counter" pairs
   * @param time
   *   time passed in ms (automatically incremented with every effect)
   * @param cache
   * @param cacheSize
   *   static cache size, doesn't change
   * @param schemaLists
   */
  case class RegistryState(
    requestsCounter: Map[String, Int],
    time: FiniteDuration,
    cache: List[(SchemaKey, SchemaCacheEntry)],
    cacheSize: Int,
    schemaLists: List[ListCacheEntry],
    schemaContentLists: Map[SchemaKey, SchemaContentListCacheEntry]
  ) {

    /** Perform a request and write it to [[requestsCounter]] */
    def request(name: String): RegistryState = {
      val i = requestsCounter.getOrElse(name, 0) + 1
      RegistryState(
        requestsCounter.updated(name, i),
        time,
        cache,
        cacheSize,
        schemaLists,
        schemaContentLists
      )
    }

    def tick = RegistryState(
      requestsCounter,
      time + 1.milli,
      cache,
      cacheSize,
      schemaLists,
      schemaContentLists
    )

    def req: Int = requestsCounter.values.sum

    override def toString: String =
      s"""requests made: $req
         |time passed:   $time
         |cache:         ${cache
          .map { case (key, (time, l)) => s"${key.toPath} ->\n\t\t\t$l (at ${time})" }
          .mkString(",")}
       """.stripMargin
  }

  object RegistryState {
    val init = RegistryState(Map.empty, 0.seconds, Nil, 0, Nil, Map.empty)
  }

  type StaticLookup[A] = State[RegistryState, A]
  type RequestsMap     = Map[String, List[Either[RegistryError, Json]]]

  object StaticLookup {

    def addTime(delta: FiniteDuration): StaticLookup[Unit] =
      State(s => (s.copy(time = s.time + delta), ()))
  }

  val staticResolverCache: CreateResolverCache[StaticLookup] =
    new CreateResolverCache[StaticLookup] {
      def createSchemaCache(
        size: Int
      ): StaticLookup[LruMap[StaticLookup, SchemaKey, SchemaCacheEntry]] =
        State(s => (s.copy(cacheSize = size), StateCache))

      def createSchemaListCache(
        size: Int
      ): StaticLookup[LruMap[StaticLookup, ListCacheKey, ListCacheEntry]] =
        State { s =>
          val cache: LruMap[StaticLookup, ListCacheKey, ListCacheEntry] = StateCacheList
          val state                                                     = s.copy(cacheSize = size)
          (state, cache)
        }

      def createSchemaContentListCache(
        size: Int
      ): StaticLookup[LruMap[StaticLookup, SchemaKey, SchemaContentListCacheEntry]] =
        State { s =>
          val cache: LruMap[StaticLookup, SchemaKey, SchemaContentListCacheEntry] =
            StataCacheSchemaContentList
          val state = s.copy(cacheSize = size)
          (state, cache)
        }

      def createMutex[K]: StaticLookup[ResolverMutex[StaticLookup, K]] =
        State.pure(stateMutex[K])
    }

  val staticClock: Clock[StaticLookup] =
    new Clock[StaticLookup] {
      override def applicative: Applicative[StaticLookup]  = Applicative[StaticLookup]
      override def monotonic: StaticLookup[FiniteDuration] = State.get.map(_.time)
      override def realTime: StaticLookup[FiniteDuration]  = State.get.map(_.time)
    }

  /** Return specified responses for HTTP repo */
  def getLookup(
    r: List[Either[RegistryError, Json]],
    l: List[SchemaKey]
  ): RegistryLookup[StaticLookup] =
    new RegistryLookup[StaticLookup] {
      def lookup(
        repositoryRef: Registry,
        schemaKey: SchemaKey
      ): StaticLookup[Either[RegistryError, Json]] =
        State { x =>
          repositoryRef match {
            case Registry.Embedded(_, _) => (x.tick, RegistryError.NotFound.asLeft)
            case Registry.Http(conf, _)  => (x.request(conf.name).tick, r(x.req))
            case Registry.InMemory(_, m) =>
              (
                x.tick,
                m.map(_.normalize).lift(x.req).toRight(RegistryError.NotFound: RegistryError)
              )
          }
        }

      def list(
        registry: Registry,
        vendor: String,
        name: String,
        model: Int
      ): StaticLookup[Either[RegistryError, SchemaList]] =
        State { x =>
          l.filter(key =>
            key.vendor == vendor && key.name == name && model == key.version.model
          ) match {
            case Nil  => (x, Left(RegistryError.NotFound))
            case keys => (x, Right(SchemaList.parseUnsafe(keys)))
          }
        }
    }

  /** Return specified responses for HTTP repo */
  def getLookupByRepo(requestsMap: RequestsMap, l: List[SchemaKey]): RegistryLookup[StaticLookup] =
    new RegistryLookup[StaticLookup] {
      def lookup(
        repositoryRef: Registry,
        key: SchemaKey
      ): StaticLookup[Either[RegistryError, Json]] =
        State { state =>
          val name = repositoryRef.config.name
          requestsMap.get(name) match {
            case Some(responses) =>
              (state.request(name).tick, responses(state.requestsCounter.getOrElse(name, 0)))
            case None =>
              (state.tick, RegistryError.NotFound.asLeft)
          }
        }

      def list(
        registry: Registry,
        vendor: String,
        name: String,
        model: Int
      ): StaticLookup[Either[RegistryError, SchemaList]] =
        State { x =>
          l.filter(key =>
            key.vendor == vendor && key.name == name && model == key.version.model
          ) match {
            case Nil  => (x, Left(RegistryError.NotFound))
            case keys => (x, Right(SchemaList.parseUnsafe(keys)))
          }
        }
    }

  private object StateCache extends LruMap[StaticLookup, SchemaKey, SchemaCacheEntry] {
    def get(key: SchemaKey): StaticLookup[Option[SchemaCacheEntry]] =
      State { state =>
        val result = state.cache.find(_._1 == key).map(_._2)
        (state.tick, result)
      }
    def put(key: SchemaKey, value: SchemaCacheEntry): StaticLookup[Unit] =
      State { state =>
        val cache = (key, value) :: state.cache.take(state.cacheSize).filterNot(_._1 == key)
        (state.copy(cache = cache).tick, ())
      }
  }

  private object StateCacheList extends LruMap[StaticLookup, ListCacheKey, ListCacheEntry] {
    def get(key: ListCacheKey): StaticLookup[Option[ListCacheEntry]] =
      State.apply[RegistryState, Option[ListCacheEntry]] { state =>
        val result = state.schemaLists.find {
          case (_, Right(list)) =>
            val head = list.schemas.head
            head.vendor == key._1 && head.name == key._2 && head.version.model == key._3
          case _ => false
        }
        (state.tick, result)
      }

    def put(key: ListCacheKey, value: ListCacheEntry): StaticLookup[Unit] =
      State { state =>
        (state.copy(schemaLists = value :: state.schemaLists).tick, ())
      }
  }

  private object StataCacheSchemaContentList
      extends LruMap[StaticLookup, SchemaKey, SchemaContentListCacheEntry] {
    def get(key: SchemaKey): StaticLookup[Option[SchemaContentListCacheEntry]] =
      State.apply[RegistryState, Option[SchemaContentListCacheEntry]] { state =>
        val result = state.schemaContentLists.get(key)
        (state.tick, result)
      }

    def put(key: SchemaKey, value: SchemaContentListCacheEntry): StaticLookup[Unit] =
      State { state =>
        (state.copy(schemaContentLists = state.schemaContentLists + (key -> value)).tick, ())
      }
  }

  private def stateMutex[K]: ResolverMutex[StaticLookup, K] =
    new ResolverMutex[StaticLookup, K] {
      def withLockOn[A](key: K)(f: => StaticLookup[A]): StaticLookup[A] =
        f
    }

  object LookupSchemasUntil {
    val vendor = "com.snowplowanalytics.iglu-test"
    val name   = "lookup-schemas-until"
    val format = "jsonschema"

    val until100 = parseSchemaUntil(1, 0, 0)
    val until110 = parseSchemaUntil(1, 1, 0)
    val until111 = parseSchemaUntil(1, 1, 1)
    val until112 = parseSchemaUntil(1, 1, 2)
    val until120 = parseSchemaUntil(1, 2, 0)
    val until121 = parseSchemaUntil(1, 2, 1)
    val until122 = parseSchemaUntil(1, 2, 2)
    val until300 = parseSchemaUntil(3, 0, 0)
    val until310 = parseSchemaUntil(3, 1, 0, "iglu-client-embedded")

    implicit val lookup: RegistryLookup[IO] = JavaNetRegistryLookup.ioLookupInstance[IO]
    def mkResolver                          = Resolver.init[IO](0, None, SpecHelpers.EmbeddedTest)

    def getUntilSchemaKey(model: Int, revision: Int, addition: Int): SchemaKey =
      SchemaKey(
        vendor,
        name,
        format,
        SchemaVer.Full(model, revision, addition)
      )

    def parseSchemaUntil(
      model: Int,
      revision: Int,
      addition: Int,
      embeddedFolder: String = "iglu-test-embedded"
    ): SelfDescribingSchema[Json] = {
      val path =
        s"/$embeddedFolder/schemas/$vendor/$name/$format/$model-$revision-$addition"
      val content = Source.fromInputStream(getClass.getResourceAsStream(path)).mkString
      parse(content) match {
        case Right(json) =>
          SelfDescribingSchema(SchemaMap(getUntilSchemaKey(model, revision, addition)), json)
        case Left(err) =>
          throw new IllegalArgumentException(s"$path can't be parsed as JSON : [$err]")
      }
    }
  }
}
