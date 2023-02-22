/*
 * Copyright (c) 2014-2023 Snowplow Analytics Ltd. All rights reserved.
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

import cats.effect.testing.specs2.CatsEffect
import com.snowplowanalytics.iglu.client.resolver.Resolver.{ResolverResult, SchemaLookupResult}
import io.circe.parser.parse

import java.net.URI
import java.time.Instant
import scala.collection.immutable.SortedMap
import scala.concurrent.duration._

// Cats
import cats.Id
import cats.effect.IO
import cats.implicits._

// circe
import io.circe.Json
import io.circe.literal._

// Iglu Core
import com.snowplowanalytics.iglu.core.{SchemaKey, SchemaVer}

// This project
import com.snowplowanalytics.iglu.client.ClientError._
import com.snowplowanalytics.iglu.client.SpecHelpers
import com.snowplowanalytics.iglu.client.resolver.ResolverSpecHelpers.StaticLookup
import com.snowplowanalytics.iglu.client.resolver.registries.RegistryLookup._
import com.snowplowanalytics.iglu.client.resolver.registries.{Registry, RegistryError}
import com.snowplowanalytics.iglu.client.resolver.registries.RegistryLookup

// Specs2
import com.snowplowanalytics.iglu.client.SpecHelpers._
import org.specs2.Specification
import org.specs2.matcher.ValidatedMatchers
import org.specs2.matcher.MatchResult

/** Like 'ResolverSpec' but using lookup methods returning 'ResolverResult' */
class ResolverResultSpec extends Specification with ValidatedMatchers with CatsEffect {
  def is = s2"""

  This is a specification to test the Resolver functionality

  our prioritizeRepos algorithm should sort repository refs in priority order $e1
  we can construct a Resolver from a valid resolver configuration JSON $e2
  a Resolver should report its failed lookups when a JSON Schema can't be resolved $e3
  a Resolver should report issues with a corrupted JSON Schema $e4
  a Resolver should report issues with invalid JSON Schema $e5
  a Resolver should respect backoff policy $e6
  a Resolver should keep retrying and never overwrite a successful response $e7
  a Resolver should accumulate errors from all repositories $e8
  we can construct a Resolver from a valid resolver 1-0-2 configuration JSON $e10
  a Resolver should cache SchemaLists with different models separately $e11
  a Resolver should not cache schema if cache is disabled $e12
  a Resolver should return cached schema when ttl not exceeded $e13
  a Resolver should return cached schema when ttl exceeded $e14
  """

  import ResolverSpec._

  def e1 = {
    val resolver =
      Resolver.init[IO](10, None, SpecHelpers.IgluCentral, Repos.one, Repos.two, Repos.three)
    val schemaKey =
      SchemaKey("de.acompany.snowplow", "mobile_context", "jsonschema", SchemaVer.Full(1, 0, 0))
    val expected: List[Registry] =
      List(Repos.two, Repos.three, Registry.EmbeddedRegistry, Repos.one, SpecHelpers.IgluCentral)

    resolver
      .map { resolver =>
        Resolver.prioritize(schemaKey.vendor, resolver.allRepos.toList) must_== expected
      }
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

    Resolver.parse[IO](config).map { parsed =>
      parsed must beRight[Resolver[IO]].like { case resolver =>
        resolver.repos must contain(SpecHelpers.IgluCentral, Repos.three)
      }
    }
  }

  def e3 = {

    val schemaKey = SchemaKey("com.acme.icarus", "wing", "jsonschema", SchemaVer.Full(1, 0, 0))
    val expected = ResolutionError(
      SortedMap(
        "Iglu Client Embedded" -> LookupHistory(Set(RegistryError.NotFound), 1, SpecHelpers.now),
        "Iglu Test Embedded"   -> LookupHistory(Set(RegistryError.NotFound), 1, SpecHelpers.now)
      )
    )

    SpecHelpers.TestResolver
      .flatMap(resolver => resolver.lookupSchema(schemaKey))
      .map { result =>
        result.leftMap(SpecHelpers.cleanTimestamps) must beLeft(expected)
      }
  }

  def e4 = {
    val schemaKey =
      SchemaKey(
        "com.snowplowanalytics.iglu-test",
        "corrupted_schema",
        "jsonschema",
        SchemaVer.Full(1, 0, 0)
      )
    val expected = ResolutionError(
      SortedMap(
        "Iglu Client Embedded" -> LookupHistory(Set(RegistryError.NotFound), 1, SpecHelpers.now),
        "Iglu Test Embedded" -> LookupHistory(
          Set(RegistryError.RepoFailure("ParsingFailure: exhausted input")),
          1,
          SpecHelpers.now
        )
      )
    )

    SpecHelpers.TestResolver
      .flatMap(resolver => resolver.lookupSchema(schemaKey))
      .map { result =>
        result.leftMap(SpecHelpers.cleanTimestamps) must beLeft(expected)
      }
  }

  def e5 = {
    val schemaKey =
      SchemaKey(
        "com.snowplowanalytics.iglu-test",
        "invalid_schema",
        "jsonschema",
        SchemaVer.Full(1, 0, 0)
      )

    SpecHelpers.TestResolver
      .flatMap(resolver => resolver.lookupSchema(schemaKey))
      .map { result =>
        result.leftMap { case ResolutionError(errors) =>
          errors.foldLeft(0) { case (acc, (_, LookupHistory(e, _, _))) => acc + e.size }
        } must beLeft(2)
      }
  }

  def e6: MatchResult[Any] = {
    val schemaKey =
      SchemaKey(
        "com.snowplowanalytics.iglu-test",
        "mock_schema",
        "jsonschema",
        SchemaVer.Full(1, 0, 0)
      )
    val timeoutError =
      RegistryError.RepoFailure("shouldn't matter").asLeft[Json]
    val correctResult =
      Json.Null.asRight[RegistryError]
    val time      = Instant.ofEpochMilli(3L)
    val responses = List(timeoutError, correctResult)

    val httpRep =
      Registry.Http(Registry.Config("Mock Repo", 1, List("com.snowplowanalytics.iglu-test")), null)

    implicit val cache = ResolverSpecHelpers.staticResolverCache
    implicit val clock = ResolverSpecHelpers.staticClock
    implicit val registryLookup: RegistryLookup[StaticLookup] =
      ResolverSpecHelpers.getLookup(responses, Nil)

    val result = for {
      resolver  <- Resolver.init[StaticLookup](10, None, httpRep)
      response1 <- resolver.lookupSchemaResult(schemaKey)
      response2 <- resolver.lookupSchemaResult(schemaKey)
      _         <- StaticLookup.addTime(600.milliseconds)
      response3 <- resolver.lookupSchemaResult(schemaKey)
    } yield (response1, response2, response3)

    val (state, (response1, response2, response3)) =
      result.run(ResolverSpecHelpers.RegistryState.init).value

    val firstFailed = response1 must beLeft[ResolutionError].like { case ResolutionError(history) =>
      history must haveValue(
        LookupHistory(Set(RegistryError.RepoFailure("shouldn't matter")), 1, time)
      )
    }
    val secondFailed = response2 must beLeft[ResolutionError].like {
      case ResolutionError(history) =>
        history must haveValue(
          LookupHistory(Set(RegistryError.RepoFailure("shouldn't matter")), 1, time)
        )
    }

    val thirdSucceeded = response3 must beRight[SchemaLookupResult].like {
      case ResolverResult.Cached(key, value, _) =>
        key must beEqualTo(schemaKey) and (value must beEqualTo(Json.Null))
    }
    val requestsNumber = state.req must beEqualTo(2)

    firstFailed and secondFailed and thirdSucceeded and requestsNumber
  }

  def e7: MatchResult[Any] = {
    val schemaKey =
      SchemaKey(
        "com.snowplowanalytics.iglu-test",
        "future_schema",
        "jsonschema",
        SchemaVer.Full(1, 0, 0)
      )
    val correctSchema = Json.Null
    val responses = List(
      RegistryError.RepoFailure("Timeout exception 1").asLeft,
      RegistryError.RepoFailure("Timeout exception 2").asLeft,
      correctSchema.asRight,
      RegistryError.RepoFailure("Should never be reached").asLeft
    )

    val httpRep =
      Registry.Http(Registry.Config("Mock Repo", 1, List("com.snowplowanalytics.iglu-test")), null)

    implicit val cache = ResolverSpecHelpers.staticResolverCache
    implicit val clock = ResolverSpecHelpers.staticClock
    implicit val registryLookup: RegistryLookup[StaticLookup] =
      ResolverSpecHelpers.getLookup(responses, Nil)

    val result = for {
      resolver <-
        Resolver
          .init[StaticLookup](
            10,
            Some(1.seconds),
            httpRep
          )
      _      <- resolver.lookupSchemaResult(schemaKey)
      _      <- StaticLookup.addTime(2.seconds)
      _      <- resolver.lookupSchemaResult(schemaKey)
      _      <- StaticLookup.addTime(2.seconds)
      _      <- resolver.lookupSchemaResult(schemaKey)
      _      <- StaticLookup.addTime(2.seconds)
      result <- resolver.lookupSchemaResult(schemaKey) // ... but don't try to overwrite it
    } yield result

    val (state, response) = result.run(ResolverSpecHelpers.RegistryState.init).value

    // Final response must not overwrite a successful one
    val finalResult = response must beRight[SchemaLookupResult].like {
      case ResolverResult.Cached(key, value, _) =>
        key must beEqualTo(schemaKey) and (value must beEqualTo(Json.Null))
    }

    // Check that it attempted to get fourth schema (500 response)
    val lookupTries = state.req must beEqualTo(4)

    finalResult and lookupTries
  }

  def e8: MatchResult[Any] = {
    val schemaKey =
      SchemaKey(
        "com.snowplowanalytics.iglu-test",
        "future_schema",
        "jsonschema",
        SchemaVer.Full(1, 0, 0)
      )
    val error1 = RegistryError.RepoFailure("Timeout exception")
    val error2 = RegistryError.RepoFailure("Network exception")
    val error3 = RegistryError.RepoFailure("Unknown exception")
    val error4 = RegistryError.RepoFailure("Server segfault")

    // Mocking repositories
    val httpRep1 = Registry.Http(
      Registry.Config("Mock Repo 1", 1, List("com.snowplowanalytics.iglu-test")),
      null
    )
    val httpRep2 = Registry.Http(
      Registry.Config("Mock Repo 2", 1, List("com.snowplowanalytics.iglu-test")),
      null
    )

    implicit val cache = ResolverSpecHelpers.staticResolverCache
    implicit val clock = ResolverSpecHelpers.staticClock
    implicit val registryLookup: RegistryLookup[StaticLookup] = ResolverSpecHelpers.getLookupByRepo(
      Map(
        "Mock Repo 1" -> List(error1.asLeft, error2.asLeft),
        "Mock Repo 2" -> List(error3.asLeft, error4.asLeft)
      ),
      Nil
    )

    val expected = ResolutionError(
      SortedMap(
        "Mock Repo 1" -> LookupHistory(Set(error1, error2), 2, Instant.ofEpochMilli(2010L)),
        "Mock Repo 2" -> LookupHistory(Set(error3, error4), 2, Instant.ofEpochMilli(2011L)),
        "Iglu Client Embedded" -> LookupHistory(
          Set(RegistryError.NotFound),
          1,
          Instant.ofEpochMilli(5L)
        )
      )
    )

    val result = for {
      resolver <- Resolver.init[StaticLookup](10, Some(100.seconds), httpRep1, httpRep2)
      _        <- resolver.lookupSchemaResult(schemaKey)
      _        <- StaticLookup.addTime(2.seconds)
      result   <- resolver.lookupSchemaResult(schemaKey)
    } yield result

    val (state, response) = result.run(ResolverSpecHelpers.RegistryState.init).value
    response must beLeft(expected) and (state.req must beEqualTo(4))
  }

  def e10: IO[MatchResult[Any]] = {

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
      .map(_ must beRight[Resolver[IO]].like { case resolver =>
        resolver.cache.flatMap(_.ttl) must beSome(10.seconds) and
          (resolver.repos must contain(SpecHelpers.IgluCentral, Repos.three))
      })
  }

  def e11 = {
    val IgluCentralServer = Registry.Http(
      Registry.Config("Iglu Central  EU1", 0, List("com.snowplowanalytics")),
      Registry
        .HttpConnection(URI.create("https://com-iglucentral-eu1-prod.iglu.snplow.net/api"), None)
    )

    val resolver = Resolver.init[Id](10, None, IgluCentralServer)

    val resultOne = resolver.listSchemasResult("com.sendgrid", "bounce", 2)
    val resultTwo = resolver.listSchemasResult("com.sendgrid", "bounce", 1)
    (resultOne, resultTwo) match {
      case (Right(one), Right(two)) => one shouldNotEqual two
      case _                        => ko("Unexpected result for two consequent listSchemas")
    }
  }

  def e12 = {

    val expectedSchema: Json =
      parse(
        scala.io.Source
          .fromInputStream(
            getClass.getResourceAsStream(
              "/iglu-test-embedded/schemas/com.snowplowanalytics.iglu-test/stock-item/jsonschema/1-0-0"
            )
          )
          .mkString
      )
        .fold(e => throw new RuntimeException(s"Cannot parse stock-item schema, $e"), identity)

    val schemaKey = SchemaKey(
      "com.snowplowanalytics.iglu-test",
      "stock-item",
      "jsonschema",
      SchemaVer.Full(1, 0, 0)
    )

    Resolver
      .init[IO](cacheSize = 0, cacheTtl = None, refs = EmbeddedTest)
      .flatMap(resolver => resolver.lookupSchemaResult(schemaKey))
      .map(_ must beRight(ResolverResult.NotCached(expectedSchema)))
  }

  def e13 = {
    val schemaKey =
      SchemaKey(
        "com.snowplowanalytics.iglu-test",
        "mock_schema",
        "jsonschema",
        SchemaVer.Full(1, 0, 0)
      )
    val schema =
      Json.Null.asRight[RegistryError]
    val responses = List(schema, schema)

    val httpRep =
      Registry.Http(Registry.Config("Mock Repo", 1, List("com.snowplowanalytics.iglu-test")), null)

    implicit val cache = ResolverSpecHelpers.staticResolverCache
    implicit val clock = ResolverSpecHelpers.staticClock
    implicit val registryLookup: RegistryLookup[StaticLookup] =
      ResolverSpecHelpers.getLookup(responses, Nil)

    val result = for {
      resolver  <- Resolver.init[StaticLookup](10, Some(200.seconds), httpRep)
      response1 <- resolver.lookupSchemaResult(schemaKey)
      _         <- StaticLookup.addTime(150.seconds) // ttl 200, delay 150
      response2 <- resolver.lookupSchemaResult(schemaKey)
    } yield (response1, response2)

    val (_, (response1, response2)) =
      result.run(ResolverSpecHelpers.RegistryState.init).value

    response1 must beRight[SchemaLookupResult].like { case _: ResolverResult.Cached[_, _] =>
      response1 must beEqualTo(
        response2
      ) // same cached (including timestamps) item because it didn't expire
    }

  }

  def e14 = {
    val schemaKey =
      SchemaKey(
        "com.snowplowanalytics.iglu-test",
        "mock_schema",
        "jsonschema",
        SchemaVer.Full(1, 0, 0)
      )
    val schema =
      Json.Null.asRight[RegistryError]
    val responses = List(schema, schema)

    val httpRep =
      Registry.Http(Registry.Config("Mock Repo", 1, List("com.snowplowanalytics.iglu-test")), null)

    implicit val cache = ResolverSpecHelpers.staticResolverCache
    implicit val clock = ResolverSpecHelpers.staticClock
    implicit val registryLookup: RegistryLookup[StaticLookup] =
      ResolverSpecHelpers.getLookup(responses, Nil)

    val result = for {
      resolver  <- Resolver.init[StaticLookup](10, Some(200.seconds), httpRep)
      response1 <- resolver.lookupSchemaResult(schemaKey)
      _         <- StaticLookup.addTime(250.seconds) // ttl 200, delay 250
      response2 <- resolver.lookupSchemaResult(schemaKey)
    } yield (response1, response2)

    val (_, (response1, response2)) =
      result.run(ResolverSpecHelpers.RegistryState.init).value

    response1 must beRight[SchemaLookupResult].like {
      case ResolverResult.Cached(_, value1, timestamp1) =>
        response2 must beRight[SchemaLookupResult].like {
          case ResolverResult.Cached(_, value2, timestamp2) =>
            value1 must beEqualTo(
              value2
            ) and (timestamp1 mustNotEqual timestamp2) // same value but different timestamps because original item expired
        }
    }
  }
}
