/*
 * Copyright (c) 2014-2020 Snowplow Analytics Ltd. All rights reserved.
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

import java.time.Instant
import java.net.URI

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
import com.snowplowanalytics.iglu.client.SpecHelpers
import com.snowplowanalytics.iglu.client.ClientError._
import com.snowplowanalytics.iglu.client.resolver.registries.{Registry, RegistryError}
import com.snowplowanalytics.iglu.client.resolver.registries.RegistryLookup._
import com.snowplowanalytics.iglu.client.resolver.ResolverSpecHelpers.StaticLookup

// Specs2
import org.specs2.Specification
import org.specs2.matcher.{DataTables, ValidatedMatchers}

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

class ResolverSpec extends Specification with DataTables with ValidatedMatchers {
  def is = s2"""

  This is a specification to test the Resolver functionality

  our prioritizeRepos algorithm should sort repository refs in priority order $e1
  we can construct a Resolver from a valid resolver configuration JSON $e2
  a Resolver should report its failed lookups when a JSON Schema can't be resolved $e3
  a Resolver should report issues with a corrupted JSON Schema $e4
  a Resolver should report issues with invalid JSON Schema $e5
  a Resolver should respect backoff policy $e6
  a Resolver should keep retrying and never overwrite a sucessful response $e7
  a Resolver should accumulate errors from all repositories $e8
  we can construct a Resolver from a valid resolver 1-0-2 configuration JSON $e10
  a Resolver should cache SchemaLists with different models separately $e11
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
    val expected = ResolutionError(
      Map(
        "Iglu Client Embedded" -> LookupHistory(Set(RegistryError.NotFound), 1, SpecHelpers.now),
        "Iglu Test Embedded"   -> LookupHistory(Set(RegistryError.NotFound), 1, SpecHelpers.now)
      ))

    SpecHelpers.TestResolver
      .flatMap(resolver => resolver.lookupSchema(schemaKey))
      .unsafeRunSync()
      .leftMap(SpecHelpers.cleanTimestamps) must beLeft(expected)
  }

  def e4 = {
    val schemaKey =
      SchemaKey(
        "com.snowplowanalytics.iglu-test",
        "corrupted_schema",
        "jsonschema",
        SchemaVer.Full(1, 0, 0))
    val expected = ResolutionError(
      Map(
        "Iglu Client Embedded" -> LookupHistory(Set(RegistryError.NotFound), 1, SpecHelpers.now),
        "Iglu Test Embedded" -> LookupHistory(
          Set(RegistryError.RepoFailure("ParsingFailure: exhausted input")),
          1,
          SpecHelpers.now)
      ))

    val result = SpecHelpers.TestResolver
      .flatMap(resolver => resolver.lookupSchema(schemaKey))
      .unsafeRunSync()
      .leftMap(SpecHelpers.cleanTimestamps)

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
      .flatMap(resolver => resolver.lookupSchema(schemaKey))
      .unsafeRunSync()
      .leftMap {
        case ResolutionError(errors) =>
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
    val time      = Instant.ofEpochMilli(2L)
    val responses = List(timeoutError, correctSchema)

    val httpRep =
      Registry.Http(Registry.Config("Mock Repo", 1, List("com.snowplowanalytics.iglu-test")), null)

    implicit val cache          = ResolverSpecHelpers.staticCache
    implicit val cacheList      = ResolverSpecHelpers.staticCacheForList
    implicit val clock          = ResolverSpecHelpers.staticClock
    implicit val registryLookup = ResolverSpecHelpers.getLookup(responses, Nil)

    val result = for {
      resolver  <- Resolver.init[StaticLookup](10, None, httpRep)
      response1 <- resolver.lookupSchema(schemaKey)
      response2 <- resolver.lookupSchema(schemaKey)
      _         <- StaticLookup.addTime(600)
      response3 <- resolver.lookupSchema(schemaKey)
    } yield (response1, response2, response3)

    val (state, (response1, response2, response3)) =
      result.run(ResolverSpecHelpers.RegistryState.init).value

    val firstFailed = response1 must beLeft.like {
      case ResolutionError(history) =>
        history must haveValue(
          LookupHistory(Set(RegistryError.RepoFailure("shouldn't matter")), 1, time))
    }
    val secondFailed = response2 must beLeft.like {
      case ResolutionError(history) =>
        history must haveValue(
          LookupHistory(Set(RegistryError.RepoFailure("shouldn't matter")), 1, time))
    }

    val thirdSucceeded = response3 must beEqualTo(correctSchema)
    val requestsNumber = state.req must beEqualTo(2)

    firstFailed and secondFailed and thirdSucceeded and requestsNumber
  }

  def e7 = {
    val schemaKey =
      SchemaKey(
        "com.snowplowanalytics.iglu-test",
        "future_schema",
        "jsonschema",
        SchemaVer.Full(1, 0, 0))
    val correctSchema = Json.Null
    val responses = List(
      RegistryError.RepoFailure("Timeout exception 1").asLeft,
      RegistryError.RepoFailure("Timeout exception 2").asLeft,
      correctSchema.asRight,
      RegistryError.RepoFailure("Should never be reached").asLeft
    )

    val httpRep =
      Registry.Http(Registry.Config("Mock Repo", 1, List("com.snowplowanalytics.iglu-test")), null)

    implicit val cache          = ResolverSpecHelpers.staticCache
    implicit val cacheList      = ResolverSpecHelpers.staticCacheForList
    implicit val clock          = ResolverSpecHelpers.staticClock
    implicit val registryLookup = ResolverSpecHelpers.getLookup(responses, Nil)

    val result = for {
      resolver <- Resolver
        .init[StaticLookup](10, Some(1000), httpRep) // FIXME: its confusing to mix millis and sec
      _      <- resolver.lookupSchema(schemaKey)
      _      <- StaticLookup.addTime(2000)
      _      <- resolver.lookupSchema(schemaKey)
      _      <- StaticLookup.addTime(2000)
      _      <- resolver.lookupSchema(schemaKey)
      _      <- StaticLookup.addTime(2000)
      result <- resolver.lookupSchema(schemaKey) // ... but don't try to overwrite it
    } yield result

    val (state, response) = result.run(ResolverSpecHelpers.RegistryState.init).value

    // Final response must not overwrite a successful one
    val finalResult = response must beRight(correctSchema)

    // Check that it attempted to get fourth schema (500 response)
    val lookupTries = state.req must beEqualTo(4)

    finalResult and lookupTries
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

    implicit val cache     = ResolverSpecHelpers.staticCache
    implicit val cacheList = ResolverSpecHelpers.staticCacheForList
    implicit val clock     = ResolverSpecHelpers.staticClock
    implicit val registryLookup = ResolverSpecHelpers.getLookupByRepo(
      Map(
        "Mock Repo 1" -> List(error1.asLeft, error2.asLeft),
        "Mock Repo 2" -> List(error3.asLeft, error4.asLeft)
      ),
      Nil)

    val expected = ResolutionError(
      Map(
        "Mock Repo 1" -> LookupHistory(Set(error1, error2), 2, Instant.ofEpochMilli(2008L)),
        "Mock Repo 2" -> LookupHistory(Set(error3, error4), 2, Instant.ofEpochMilli(2009L)),
        "Iglu Client Embedded" -> LookupHistory(
          Set(RegistryError.NotFound),
          1,
          Instant.ofEpochMilli(4L))
      ))

    val result = for {
      resolver <- Resolver.init[StaticLookup](10, Some(100), httpRep1, httpRep2)
      _        <- resolver.lookupSchema(schemaKey)
      _        <- StaticLookup.addTime(2000)
      result   <- resolver.lookupSchema(schemaKey)
    } yield result

    val (state, response) = result.run(ResolverSpecHelpers.RegistryState.init).value
    response must beLeft(expected) and (state.req must beEqualTo(4))
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

  def e11 = {
    val IgluCentralServer = Registry.Http(
      Registry.Config("Iglu Central  EU1", 0, List("com.snowplowanalytics")),
      Registry
        .HttpConnection(URI.create("https://com-iglucentral-eu1-prod.iglu.snplow.net/api"), None)
    )

    val resolver = Resolver.init[Id](10, None, IgluCentralServer)

    val resultOne = resolver.listSchemas("com.sendgrid", "bounce", 2)
    val resultTwo = resolver.listSchemas("com.sendgrid", "bounce", 1)
    (resultOne, resultTwo) match {
      case (Right(one), Right(two)) => one shouldNotEqual (two)
      case _                        => ko("Unexpected result for two consequent listSchemas")
    }
  }
}
