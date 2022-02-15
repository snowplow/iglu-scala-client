/*
 * Copyright (c) 2018-2022 Snowplow Analytics Ltd. All rights reserved.
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

// Cats
import cats.Applicative
import cats.data.State
import cats.effect.Clock
import cats.syntax.either._

import scala.concurrent.duration.{DurationInt, FiniteDuration}

// Circe
import io.circe.Json

// LRU Map
import com.snowplowanalytics.iglu.core.circe.implicits._
import com.snowplowanalytics.iglu.core.{SchemaKey, SchemaList}
import com.snowplowanalytics.lrumap.{CreateLruMap, LruMap}

// This project
import com.snowplowanalytics.iglu.client.resolver.registries.{
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
    schemaLists: List[ListCacheEntry]
  ) {

    /** Perform a request and write it to [[requestsCounter]] */
    def request(name: String): RegistryState = {
      val i = requestsCounter.getOrElse(name, 0) + 1
      RegistryState(requestsCounter.updated(name, i), time, cache, cacheSize, schemaLists)
    }

    def tick = RegistryState(requestsCounter, time + 1.milli, cache, cacheSize, schemaLists)

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
    val init = RegistryState(Map.empty, 0.seconds, Nil, 0, Nil)
  }

  type StaticLookup[A] = State[RegistryState, A]
  type RequestsMap     = Map[String, List[Either[RegistryError, Json]]]

  object StaticLookup {

    def addTime(delta: FiniteDuration): StaticLookup[Unit] =
      State(s => (s.copy(time = s.time + delta), ()))
  }

  val staticCache: InitSchemaCache[StaticLookup] =
    new CreateLruMap[StaticLookup, SchemaKey, SchemaCacheEntry] {
      def create(size: Int): StaticLookup[LruMap[StaticLookup, SchemaKey, SchemaCacheEntry]] =
        State(s => (s.copy(cacheSize = size), StateCache))
    }

  val staticCacheForList: InitListCache[StaticLookup] =
    new CreateLruMap[StaticLookup, ListCacheKey, ListCacheEntry] {
      def create(
        size: Int
      ): StaticLookup[LruMap[StaticLookup, ListCacheKey, ListCacheEntry]] =
        State { s =>
          val cache: LruMap[StaticLookup, ListCacheKey, ListCacheEntry] = StateCacheList
          val state                                                     = s.copy(cacheSize = size)
          (state, cache)
        }
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
            case keys => (x, Right(SchemaList(keys)))
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
            case keys => (x, Right(SchemaList(keys)))
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
}
