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
package com.snowplowanalytics.iglu.client.resolver

import cats.effect.Clock
import cats.implicits._
import com.snowplowanalytics.iglu.client.resolver.ResolverSpecHelpers._
import com.snowplowanalytics.iglu.client.resolver.registries.{Registry, RegistryError}
import com.snowplowanalytics.iglu.core.{SchemaKey, SchemaVer}
import io.circe.Json
import org.specs2.Specification
import org.specs2.matcher.MatchResult

import java.time.Instant
import scala.concurrent.duration.DurationInt

class ResolverCacheSpec extends Specification {
  def is = s2"""
  Disallow overwriting successful request with failed one $e1
  Combine failures during putSchema $e2
  Should create a resolver cache if cache size > 0 and TTL is none or more than 0 $e3
  """

  def e1: MatchResult[Product] = {
    import ResolverCacheSpec._

    val key          = SchemaKey("com.acme", "schema", "jsonschema", SchemaVer.Full(1, 0, 0))
    val schema       = Json.Null
    val lookupResult = schema.asRight[LookupFailureMap]

    val expectedState =
      RegistryState(Map.empty, 4.millis, List((key, (2.millis, Right(Json.Null)))), 5, List())

    val test = for {
      cache <- ResolverCache.init[StaticLookup](5, Some(10.seconds))
      cacheUnsafe = cache.getOrElse(throw new RuntimeException("Cache cannot be created"))
      _      <- cacheUnsafe.putSchema(key, lookupResult)
      result <- cacheUnsafe.putSchema(key, Map.empty[Registry, LookupHistory].asLeft[Json])
    } yield result

    val (state, result) = test.run(RegistryState.init).value
    val schemaResult    = result must beRight(schema)
    val stateResult     = state must beEqualTo(expectedState)

    schemaResult and stateResult
  }

  def e2: MatchResult[Any] = {
    import ResolverCacheSpec._

    val key = SchemaKey("com.acme", "schema", "jsonschema", SchemaVer.Full(1, 0, 0))
    val failure1: LookupFailureMap = Map(
      Registry.Http(Registry.Config("one", 1, List.empty), null) -> LookupHistory(
        Set(RegistryError.NotFound),
        1,
        Instant.ofEpochMilli(10000)
      ),
      Registry.Http(Registry.Config("two", 1, List.empty), null) -> LookupHistory(
        Set(RegistryError.NotFound),
        2,
        Instant.ofEpochMilli(10005)
      )
    )
    val failure2: LookupFailureMap = Map(
      Registry.Http(Registry.Config("one", 1, List.empty), null) -> LookupHistory(
        Set(RegistryError.RepoFailure("Doesn't matter")),
        4,
        Instant.ofEpochMilli(10000)
      )
    )

    val expectedLookup: LookupFailureMap = Map(
      Registry.Http(Registry.Config("one", 1, List()), null) -> LookupHistory(
        Set(RegistryError.NotFound, RegistryError.RepoFailure("Doesn't matter")),
        4,
        Instant.ofEpochMilli(10000)
      ),
      Registry.Http(Registry.Config("two", 1, List()), null) -> LookupHistory(
        Set(RegistryError.NotFound),
        2,
        Instant.ofEpochMilli(10005)
      )
    )

    val test = for {
      cache <- ResolverCache.init[StaticLookup](5, Some(10.seconds))
      cacheUnsafe = cache.getOrElse(throw new RuntimeException("Cache cannot be created"))
      _      <- cacheUnsafe.putSchema(key, failure1.asLeft[Json])
      result <- cacheUnsafe.putSchema(key, failure2.asLeft[Json])
    } yield result

    val (_, result: SchemaLookup) = test.run(RegistryState.init).value
    // something odd happens with implicit conversions on scala3
    result must_== Left(expectedLookup)
  }

  def e3 = {
    ResolverCache.shouldCreateResolverCache(10, Some(10.seconds)) mustEqual true
    ResolverCache.shouldCreateResolverCache(10, None) mustEqual true
    ResolverCache.shouldCreateResolverCache(0, Some(10.seconds)) mustEqual false
    ResolverCache.shouldCreateResolverCache(0, None) mustEqual false
    ResolverCache.shouldCreateResolverCache(10, Some(-10.seconds)) mustEqual false
  }

}

object ResolverCacheSpec {
  // No need to overwrite anything
  implicit val a: InitSchemaCache[StaticLookup] = staticCache
  implicit val c: InitListCache[StaticLookup]   = staticCacheForList
  implicit val b: Clock[StaticLookup]           = staticClock
}
