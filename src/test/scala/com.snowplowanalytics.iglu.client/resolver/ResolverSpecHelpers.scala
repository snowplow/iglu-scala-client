package com.snowplowanalytics.iglu.client
package resolver

// Cats
import cats.syntax.either._
import cats.data.State
import cats.effect.Clock

import scala.concurrent.duration.TimeUnit

// Circe
import io.circe.Json

// LRU Map
import com.snowplowanalytics.lrumap.{CreateLruMap, LruMap}

import com.snowplowanalytics.iglu.core.{SchemaKey, SchemaList}
import com.snowplowanalytics.iglu.core.circe.implicits._

// This project
import resolver.registries.Registry
import resolver.registries.RegistryError
import resolver.registries.RegistryLookup

object ResolverSpecHelpers {
  case class RegistryState(
    requestsCounter: Map[String, Int],
    time: Int,
    cache: List[(SchemaKey, SchemaLookupStamped)],
    cacheSize: Int,
    schemaLists: List[SchemaList]) {
    def request(name: String) = {
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

  case class TestState[A](unwrap: State[RegistryState, A])

  val staticCache: InitSchemaCache[StaticLookup] =
    new CreateLruMap[StaticLookup, SchemaKey, SchemaLookupStamped] {
      def create(size: Int): StaticLookup[LruMap[StaticLookup, SchemaKey, SchemaLookupStamped]] =
        State(s => (s.copy(cacheSize = size), StateCache))
    }
  val staticCacheForList: CreateLruMap[StaticLookup, (String, String), SchemaList] =
    new CreateLruMap[StaticLookup, (String, String), SchemaList] {
      def create(size: Int): StaticLookup[LruMap[StaticLookup, (String, String), SchemaList]] =
        State(s => (s.copy(cacheSize = size), StateCacheList))
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

      def list(registry: Registry, vendor: String, name: String): StaticLookup[Option[SchemaList]] =
        State { x =>
          l.filter(key => key.vendor == vendor && key.name == name) match {
            case Nil  => (x, None)
            case keys => (x, Some(SchemaList(keys)))
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

      def list(registry: Registry, vendor: String, name: String): StaticLookup[Option[SchemaList]] =
        State { x =>
          l.filter(key => key.vendor == vendor && key.name == name) match {
            case Nil  => (x, None)
            case keys => (x, Some(SchemaList(keys)))
          }
        }
    }

  private object StateCache extends LruMap[StaticLookup, SchemaKey, SchemaLookupStamped] {
    def get(key: SchemaKey): StaticLookup[Option[SchemaLookupStamped]] = State { state =>
      val result = state.cache.find(_._1 == key).map(_._2)
      (state.tick, result)
    }
    def put(key: SchemaKey, value: (Int, SchemaLookup)): StaticLookup[Unit] = State { state =>
      val cache = (key, value) :: state.cache.take(state.cacheSize).filterNot(_._1 == key)
      (state.copy(cache = cache).tick, ())
    }
  }

  private object StateCacheList extends LruMap[StaticLookup, (String, String), SchemaList] {
    def get(key: (String, String)): StaticLookup[Option[SchemaList]] = State { state =>
      val result = state.schemaLists.find { list =>
        val SchemaKey(v, n, _, _) = list.schemas.head
        v == key._1 && n == key._2
      }
      (state.tick, result)
    }

    def put(key: (String, String), value: SchemaList): StaticLookup[Unit] = State { state =>
      (state.copy(schemaLists = value :: state.schemaLists).tick, ())
    }
  }
}
