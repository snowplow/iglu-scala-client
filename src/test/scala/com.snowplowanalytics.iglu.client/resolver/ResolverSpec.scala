/*
 * Copyright (c) 2014-2018 Snowplow Analytics Ltd. All rights reserved.
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

// Cats
import cats.effect.IO
import cats.implicits._
import com.snowplowanalytics.iglu.client.resolver.ResolverSpecHelpers.StaticLookup

// circe
import io.circe.Json
import io.circe.literal._

// Iglu Core
import com.snowplowanalytics.iglu.core.{SchemaKey, SchemaVer}

// This project
import com.snowplowanalytics.iglu.client.SpecHelpers
import com.snowplowanalytics.iglu.client.ClientError._
import com.snowplowanalytics.iglu.client.resolver.registries.{Registry, RegistryError}
import com.snowplowanalytics.iglu.client.resolver.registries.RegistryLookup._

// Specs2
import org.specs2.Specification
import org.specs2.matcher.{DataTables, ValidatedMatchers}
import org.specs2.mock.Mockito

import com.snowplowanalytics.iglu.client.SpecHelpers._

object ResolverSpec {

  object Repos {

    private val embedRef: (String, Int) => Registry.Embedded = (prefix, priority) =>
      Registry.Embedded(Registry.Config("An embedded repo", priority, List(prefix)), "/embed-path")

    val one: Registry.Embedded   = embedRef("com.acme", 0)
    val two: Registry.Embedded   = embedRef("de.acompany.snowplow", 40)
    val three: Registry.Embedded = embedRef("de.acompany.snowplow", 100)
  }
}

class ResolverSpec extends Specification with DataTables with ValidatedMatchers with Mockito {
  def is = s2"""

  This is a specification to test the Resolver functionality

  our prioritizeRepos algorithm should sort repository refs in priority order $e1
  we can construct a Resolver from a valid resolver configuration JSON $e2
  a Resolver should report its failed lookups when a JSON Schema can't be resolved $e3
  a Resolver should report issues with a corrupted JSON Schema $e4
  a Resolver should report issues with invalid JSON Schema $e5
  a Resolver should retry after non-404 errors $e6
  a Resolver should give up after 3rd retry $e7
  a Resolver should accumulate errors from all repositories $e8
  a Resolver should respect cacheTtl $e9
  we can construct a Resolver from a valid resolver 1-0-2 configuration JSON $e10
  """

  import ResolverSpec._

  def e1 = {
    val resolver =
      Resolver.init[IO](cacheSize = 10, SpecHelpers.IgluCentral, Repos.one, Repos.two, Repos.three)
    val schemaKey =
      SchemaKey("de.acompany.snowplow", "mobile_context", "jsonschema", SchemaVer.Full(1, 0, 0))
    val expected: List[Registry] =
      List(Repos.two, Repos.three, Registry.EmbeddedRegistry, Repos.one, SpecHelpers.IgluCentral)

    resolver
      .map { resolver =>
        Resolver.prioritizeRepos(schemaKey, resolver.allRepos.toList) must_== expected
      }
      .unsafeRunSync()
  }

  def e2 = {

    val config =
      json"""{
            "schema": "iglu:com.snowplowanalytics.iglu/resolver-config/jsonschema/1-0-0",
            "data": {
              "cacheSize": 500,
              "repositories": [
                {
                  "name": "Iglu Central",
                  "priority": 0,
                  "vendorPrefixes": [ "com.snowplowanalytics" ],
                  "connection": {
                    "http": {
                      "uri": "http://iglucentral.com"
                    }
                  }
                }, {
                  "name": "An embedded repo",
                  "priority": 100,
                  "vendorPrefixes": [ "de.acompany.snowplow" ],
                  "connection": {
                    "embedded": {
                      "path": "/embed-path"
                    }
                  }
                }
              ]
            }
          }"""

    val result = for {
      parsed <- Resolver.parse[IO](config)
    } yield
      parsed must beRight.like {
        case resolver => resolver.repos must contain(SpecHelpers.IgluCentral, Repos.three)
      }

    result.unsafeRunSync()
  }

  def e3 = {

    val schemaKey = SchemaKey("com.acme.icarus", "wing", "jsonschema", SchemaVer.Full(1, 0, 0))
    val expected = ResolverError(
      Map(
        "Iglu Client Embedded" -> LookupHistory(Set(RegistryError.NotFound), 1, false),
        "Iglu Test Embedded"   -> LookupHistory(Set(RegistryError.NotFound), 1, false)
      ))

    SpecHelpers.TestResolver
      .flatMap(resolver => resolver.lookupSchema(schemaKey, 3))
      .unsafeRunSync() must beLeft(expected)
  }

  def e4 = {
    val schemaKey =
      SchemaKey(
        "com.snowplowanalytics.iglu-test",
        "corrupted_schema",
        "jsonschema",
        SchemaVer.Full(1, 0, 0))
    val expected = ResolverError(
      Map(
        "Iglu Client Embedded" -> LookupHistory(Set(RegistryError.NotFound), 1, false),
        "Iglu Test Embedded" -> LookupHistory(
          Set(RegistryError.RepoFailure("ParsingFailure: exhausted input")),
          1,
          false)
      ))

    val result = SpecHelpers.TestResolver
      .flatMap(resolver => resolver.lookupSchema(schemaKey, 3))
      .unsafeRunSync()

    result must beLeft(expected)
  }

  def e5 = {
    val schemaKey =
      SchemaKey(
        "com.snowplowanalytics.iglu-test",
        "invalid_schema",
        "jsonschema",
        SchemaVer.Full(1, 0, 0))

    SpecHelpers.TestResolver
      .flatMap(resolver => resolver.lookupSchema(schemaKey, 3))
      .unsafeRunSync()
      .leftMap {
        case ResolverError(errors) =>
          errors.foldLeft(0) { case (acc, (_, LookupHistory(e, _, _))) => acc + e.size }
        case _ => 0
      } must beLeft(2)
  }

  def e6 = {
    val schemaKey =
      SchemaKey(
        "com.snowplowanalytics.iglu-test",
        "mock_schema",
        "jsonschema",
        SchemaVer.Full(1, 0, 0))
    val timeoutError =
      RegistryError.RepoFailure("shouldn't matter").asLeft[Json]
    val correctSchema =
      Json.Null.asRight[RegistryError]

    val httpRep =
      Registry.Http(Registry.Config("Mock Repo", 1, List("com.snowplowanalytics.iglu-test")), null)

    val responses = List(timeoutError, correctSchema)

    implicit val registryLookup = ResolverSpecHelpers.getLookup(responses)
    implicit val cache          = ResolverSpecHelpers.staticCache
    implicit val clock          = ResolverSpecHelpers.staticClock

    val result = for {
      resolver  <- Resolver.init[StaticLookup](10, httpRep)
      response1 <- resolver.lookupSchema(schemaKey, 3)
      response2 <- resolver.lookupSchema(schemaKey, 3)
    } yield (response1, response2)

    val (state, (response1, response2)) = result.run(ResolverSpecHelpers.RegistryState.init).value

    val firstFailed = response1 must beLeft.like {
      case ResolverError(history) =>
        history must haveValue(
          LookupHistory(Set(RegistryError.RepoFailure("shouldn't matter")), 1, false))
    }
    val secondSucceeded = response2 must beEqualTo(correctSchema)
    val requestsNumber  = state.req must beEqualTo(2)

    firstFailed and secondSucceeded and requestsNumber
  }

  def e7 = {
    val schemaKey =
      SchemaKey(
        "com.snowplowanalytics.iglu-test",
        "future_schema",
        "jsonschema",
        SchemaVer.Full(1, 0, 0))
    val timeout       = RegistryError.RepoFailure("Timeout exception")
    val correctSchema = Json.Null
    val httpRep =
      Registry.Http(Registry.Config("Mock Repo", 1, List("com.snowplowanalytics.iglu-test")), null)

    val responses = List(
      timeout.asLeft[Json],
      timeout.asLeft[Json],
      timeout.asLeft[Json],
      correctSchema.asRight // Should never be reached
    )

    implicit val registryLookup = ResolverSpecHelpers.getLookup(responses)
    implicit val cache          = ResolverSpecHelpers.staticCache
    implicit val clock          = ResolverSpecHelpers.staticClock

    val result = for {
      resolver <- Resolver.init[StaticLookup](10, httpRep)
      _        <- resolver.lookupSchema(schemaKey, 3)
      _        <- resolver.lookupSchema(schemaKey, 3)
      _        <- resolver.lookupSchema(schemaKey, 3)
      _        <- resolver.lookupSchema(schemaKey, 3) // this and subsequent return error
      result   <- resolver.lookupSchema(schemaKey, 3)
    } yield result

    val (state, response) = result.run(ResolverSpecHelpers.RegistryState.init).value

    // Only 3 attempts were made to Mock Repo and it is reflected in LookupHistory
    val lookupHistory = response must beLeft.like {
      case ResolverError(history) =>
        history must havePair("Mock Repo" -> LookupHistory(Set(timeout), 3, false))
    }

    // Only 3 attempts were made to HTTP Repo and it is reflected in State
    val lookupTries = state.req must beEqualTo(3)

    // (get+put+embd+http) + 2*(get+put+http) + 2*(get+put)
    val timePassed = state.time must beEqualTo(14)

    lookupHistory and lookupTries and timePassed
  }

  def e8 = {
    val schemaKey =
      SchemaKey(
        "com.snowplowanalytics.iglu-test",
        "future_schema",
        "jsonschema",
        SchemaVer.Full(1, 0, 0))
    val error1 = RegistryError.RepoFailure("Timeout exception")
    val error2 = RegistryError.RepoFailure("Network exception")
    val error3 = RegistryError.RepoFailure("Unknown exception")
    val error4 = RegistryError.RepoFailure("Server segfault")

    // Mocking repositories
    val httpRep1 = Registry.Http(
      Registry.Config("Mock Repo 1", 1, List("com.snowplowanalytics.iglu-test")),
      null)
    val httpRep2 = Registry.Http(
      Registry.Config("Mock Repo 2", 1, List("com.snowplowanalytics.iglu-test")),
      null)

    implicit val cache = ResolverSpecHelpers.staticCache
    implicit val clock = ResolverSpecHelpers.staticClock
    implicit val registryLookup = ResolverSpecHelpers.getLookupByRepo(
      Map(
        "Mock Repo 1" -> List(error1.asLeft, error2.asLeft),
        "Mock Repo 2" -> List(error3.asLeft, error4.asLeft)
      ))

    val expected = ResolverError(
      Map(
        "Mock Repo 1"          -> LookupHistory(Set(error2, error1), 2, false),
        "Mock Repo 2"          -> LookupHistory(Set(error4, error3), 2, false),
        "Iglu Client Embedded" -> LookupHistory(Set(RegistryError.NotFound), 1, false)
      ))

    val result = for {
      resolver <- Resolver.init[StaticLookup](10, httpRep1, httpRep2)
      _        <- resolver.lookupSchema(schemaKey, 3)
      result   <- resolver.lookupSchema(schemaKey, 3)
    } yield result

    val (state, response) = result.run(ResolverSpecHelpers.RegistryState.init).value
    response must beLeft(expected) and (state.req must beEqualTo(4))
  }

  def e9 = {
    val schemaKey =
      SchemaKey(
        "com.snowplowanalytics.iglu-test",
        "future_schema",
        "jsonschema",
        SchemaVer.Full(1, 0, 0))
    val correctSchema =
      json"""{
       	"$$schema": "http://iglucentral.com/schemas/com.snowplowanalytics.self-desc/schema/jsonschema/1-0-0#",
       	"self": {
       		"vendor": "com.snowplowanalytics.iglu-test",
       		"name": "future_schema",
       		"format": "jsonschema",
       		"version": "1-0-0"
        }
       }""".asRight

    implicit val cache = ResolverSpecHelpers.staticCache
    implicit val clock = ResolverSpecHelpers.staticClock
    implicit val registryLookup = ResolverSpecHelpers.getLookupByRepo(
      Map(
        "Mock Repo" -> List(
          RegistryError.NotFound.asLeft[Json],
          correctSchema
        )))

    // Mocking repository
    val httpRep =
      Registry.Http(Registry.Config("Mock Repo", 1, List("com.snowplowanalytics.iglu-test")), null)

    val result = for {
      resolver <- SchemaCache
        .init[StaticLookup](10, Some(3))
        .map(cache => Resolver(List(httpRep), cache))
      firstResult       <- resolver.lookupSchema(schemaKey, 3) // not found
      immediateResult   <- resolver.lookupSchema(schemaKey, 3) // not found, but from cache, not from RegistryRef
      _                 <- cats.data.State.modify[ResolverSpecHelpers.RegistryState](s => s.tick.tick)
      afterCacheExpired <- resolver.lookupSchema(schemaKey, 3) // invalidate cache, retry and succeed
    } yield (firstResult, immediateResult, afterCacheExpired)

    val (state, (firstResult, immediateResult, afterCacheExpired)) =
      result.run(ResolverSpecHelpers.RegistryState.init).value

    ((firstResult must beLeft)
      and (immediateResult must beLeft)
      and (afterCacheExpired must beEqualTo(correctSchema))
      and (state.req must beEqualTo(2)))
  }

  def e10 = {

    val config =
      json"""{
          "schema": "iglu:com.snowplowanalytics.iglu/resolver-config/jsonschema/1-0-2",
          "data": {
            "cacheSize": 500,
            "cacheTtl": 10,
            "repositories": [
              {
                "name": "Iglu Central",
                "priority": 0,
                "vendorPrefixes": [ "com.snowplowanalytics" ],
                "connection": {
                  "http": {
                    "uri": "http://iglucentral.com",
                    "apikey": null
                  }
                }
              }, {
                "name": "An embedded repo",
                "priority": 100,
                "vendorPrefixes": [ "de.acompany.snowplow" ],
                "connection": {
                  "embedded": {
                    "path": "/embed-path"
                  }
                }
              }
            ]
          }
          }"""

    Resolver
      .parse[IO](config)
      .map(_ must beRight.like {
        case resolver =>
          resolver.cache.flatMap(_.ttl) must beSome(10) and
            (resolver.repos must contain(SpecHelpers.IgluCentral, Repos.three))
      })
      .unsafeRunSync()
  }
}
