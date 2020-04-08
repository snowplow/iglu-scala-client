/*
 * Copyright (c) 2018-2019 Snowplow Analytics Ltd. All rights reserved.
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
import cats.data.State
import cats.syntax.either._
import cats.effect.Clock

import scala.concurrent.duration.TimeUnit

// Circe
import io.circe.Json

// LRU Map
import com.snowplowanalytics.lrumap.{CreateLruMap, LruMap}

import com.snowplowanalytics.iglu.core.{SchemaKey, SchemaList}
import com.snowplowanalytics.iglu.core.circe.implicits._

// This project
import resolver.registries.{Registry, RegistryError, RegistryLookup}

object ResolverSpecHelpers {

  /**
   * State of the "resolver's world" to test every aspect of the resolution in a pure FP way
   * @param requestsCounter map of "registry name -> counter" pairs
   * @param time time passed in ms (automatically incremented with every effect)
   * @param cache
   * @param cacheSize static cache size, doesn't change
   * @param schemaLists
   */
  case class RegistryState(
    requestsCounter: Map[String, Int],
    time: Int,
    cache: List[(SchemaKey, (Int, SchemaLookup))],
    cacheSize: Int,
    schemaLists: List[(Int, ListLookup)]) {

    /** Perform a request and write it to [[requestsCounter]] */
    def request(name: String): RegistryState = {
      val i = requestsCounter.getOrElse(name, 0) + 1
      RegistryState(requestsCounter.updated(name, i), time, cache, cacheSize, schemaLists)
    }

    def tick = RegistryState(requestsCounter, time + 1, cache, cacheSize, schemaLists)

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
    val init = RegistryState(Map.empty, 0, Nil, 0, Nil)
  }

  type StaticLookup[A] = State[RegistryState, A]
  type RequestsMap     = Map[String, List[Either[RegistryError, Json]]]

  object StaticLookup {

    /** Either logical clock unit or milliseconds for some specs */
    def addTime(delta: Int): StaticLookup[Unit] =
      State(s => (s.copy(time = s.time + delta), ()))

    def getTime: StaticLookup[Int] =
      State.get[RegistryState].map(_.time)
  }

  val staticCache: InitSchemaCache[StaticLookup] =
    new CreateLruMap[StaticLookup, SchemaKey, (Int, SchemaLookup)] {
      def create(size: Int): StaticLookup[LruMap[StaticLookup, SchemaKey, (Int, SchemaLookup)]] =
        State(s => (s.copy(cacheSize = size), StateCache))
    }

  val staticCacheForList: InitListCache[StaticLookup] =
    new CreateLruMap[StaticLookup, (String, String, Int), (Int, ListLookup)] {
      def create(
        size: Int): StaticLookup[LruMap[StaticLookup, (String, String, Int), (Int, ListLookup)]] =
        State { s =>
          val cache: LruMap[StaticLookup, (String, String, Int), (Int, ListLookup)] = StateCacheList
          val state                                                                 = s.copy(cacheSize = size)
          (state, cache)
        }
    }

  val staticClock: Clock[StaticLookup] =
    new Clock[StaticLookup] {
      def realTime(unit: TimeUnit): StaticLookup[Long]  = State.get.map(_.time.toLong)
      def monotonic(unit: TimeUnit): StaticLookup[Long] = State.get.map(_.time.toLong)
    }

  /** Return specified responses for HTTP repo */
  def getLookup(
    r: List[Either[RegistryError, Json]],
    l: List[SchemaKey]): RegistryLookup[StaticLookup] =
    new RegistryLookup[StaticLookup] {
      def lookup(
        repositoryRef: Registry,
        schemaKey: SchemaKey): StaticLookup[Either[RegistryError, Json]] =
        State { x =>
          repositoryRef match {
            case Registry.Embedded(_, _) => (x.tick, RegistryError.NotFound.asLeft)
            case Registry.Http(conf, _)  => (x.request(conf.name).tick, r(x.req))
            case Registry.InMemory(_, m) =>
              (
                x.tick,
                m.map(_.normalize).lift(x.req).toRight(RegistryError.NotFound: RegistryError))
          }
        }

      def list(
        registry: Registry,
        vendor: String,
        name: String,
        model: Int): StaticLookup[Either[RegistryError, SchemaList]] =
        State { x =>
          l.filter(key => key.vendor == vendor && key.name == name && model == key.version.model) match {
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
        key: SchemaKey): StaticLookup[Either[RegistryError, Json]] =
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
        model: Int): StaticLookup[Either[RegistryError, SchemaList]] =
        State { x =>
          l.filter(key => key.vendor == vendor && key.name == name && model == key.version.model) match {
            case Nil  => (x, Left(RegistryError.NotFound))
            case keys => (x, Right(SchemaList(keys)))
          }
        }
    }

  private object StateCache extends LruMap[StaticLookup, SchemaKey, (Int, SchemaLookup)] {
    def get(key: SchemaKey): StaticLookup[Option[(Int, SchemaLookup)]] = State { state =>
      val result = state.cache.find(_._1 == key).map(_._2)
      (state.tick, result)
    }
    def put(key: SchemaKey, value: (Int, SchemaLookup)): StaticLookup[Unit] = State { state =>
      val cache = (key, value) :: state.cache.take(state.cacheSize).filterNot(_._1 == key)
      (state.copy(cache = cache).tick, ())
    }
  }

  private object StateCacheList
      extends LruMap[StaticLookup, (String, String, Int), (Int, ListLookup)] {
    def get(key: (String, String, Int)): StaticLookup[Option[(Int, ListLookup)]] =
      State.apply[RegistryState, Option[(Int, ListLookup)]] { state =>
        val result = state.schemaLists.find {
          case (_, Right(list)) =>
            val head = list.schemas.head
            head.vendor == key._1 && head.name == key._2 && head.version.model == key._3
          case _ => false
        }
        (state.tick, result)
      }

    def put(key: (String, String, Int), value: (Int, ListLookup)): StaticLookup[Unit] = State {
      state =>
        (state.copy(schemaLists = value :: state.schemaLists).tick, ())
    }
  }
}
