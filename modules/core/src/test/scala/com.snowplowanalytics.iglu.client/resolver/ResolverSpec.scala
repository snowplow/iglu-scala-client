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

import com.snowplowanalytics.iglu.core.SchemaList

import java.net.URI
import java.time.Instant
import scala.collection.immutable.SortedMap
import scala.concurrent.duration.DurationInt

// Cats
import cats.Id
import cats.effect.IO
import cats.effect.Clock
import cats.syntax.all._

// circe
import io.circe.Json
import io.circe.literal._

// Iglu Core
import com.snowplowanalytics.iglu.core.{SchemaKey, SchemaVer}

// This project
import com.snowplowanalytics.iglu.client.ClientError._
import com.snowplowanalytics.iglu.client.SpecHelpers
import com.snowplowanalytics.iglu.client.resolver.ResolverSpecHelpers.StaticLookup
import com.snowplowanalytics.iglu.client.resolver.registries.JavaNetRegistryLookup._
import com.snowplowanalytics.iglu.client.resolver.registries.{Registry, RegistryError}
import com.snowplowanalytics.iglu.client.resolver.registries.RegistryLookup

// Specs2
import cats.effect.testing.specs2.CatsEffect
import com.snowplowanalytics.iglu.client.SpecHelpers._
import org.specs2.Specification
import org.specs2.matcher.MatchResult

object ResolverSpec {

  object Repos {

    private val embedRef: (String, Int) => Registry.Embedded = (prefix, priority) =>
      Registry.Embedded(Registry.Config("An embedded repo", priority, List(prefix)), "/embed-path")

    val one: Registry.Embedded   = embedRef("com.acme", 0)
    val two: Registry.Embedded   = embedRef("de.acompany.snowplow", 40)
    val three: Registry.Embedded = embedRef("de.acompany.snowplow", 100)

    val custom: Registry =
      Registry.Http(
        Registry.Config("Iglu Custom Repo", 10, List("com.acme")),
        Registry.HttpConnection(URI.create("http://iglu.acme.com"), None)
      )

    val httpRep =
      Registry.Http(
        Registry.Config("Mock Repo", 1, List("com.snowplowanalytics.iglu-test")),
        Registry.HttpConnection(URI.create("http://iglu.mock.org"), None)
      )
  }
}

class ResolverSpec extends Specification with CatsEffect {
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
  a Resolver should use schemaKey provided in SchemaListLike for result validation $e12
  result from SchemaListLike should contain the exact schemaKey provided $e13
  isNotFound should
    return true if custom repo and Iglu Central repos don't have a schema $e14
    return true if one Iglu Central repo returns a RepoFailure and the other one NotFound $e15
    return false if custom repo returns a RepoFailure $e16
    return true if custom repo returns a ClientFailure $e17
    return true if there is no custom repo, one Iglu Central repo returns an error and the other one NotFound $e18
    return false if there is no custom repo and Iglu Central ones return a RepoFailure $e19
    return true if there is no custom repo and Iglu Central ones return a ClientFailure $e20
    return true if there is just one custom repo that returns NotFound $e21
    return false if there is just one custom repo that returns a RepoFailure $e22
    return true if there is just one custom repo that returns a ClientFailure $e23
    return true if one Iglu Central repo returns 2 errors and the other one returns one error and one NotFound $e24
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
    val correctSchema =
      Json.Null.asRight[RegistryError]
    val time      = Instant.ofEpochMilli(3L)
    val responses = List(timeoutError, correctSchema)

    implicit val cache: CreateResolverCache[StaticLookup] = ResolverSpecHelpers.staticResolverCache
    implicit val clock: Clock[StaticLookup]               = ResolverSpecHelpers.staticClock
    implicit val registryLookup: RegistryLookup[StaticLookup] =
      ResolverSpecHelpers.getLookup(responses, Nil)

    val result = for {
      resolver  <- Resolver.init[StaticLookup](10, None, Repos.httpRep)
      response1 <- resolver.lookupSchema(schemaKey)
      response2 <- resolver.lookupSchema(schemaKey)
      _         <- StaticLookup.addTime(600.millis)
      response3 <- resolver.lookupSchema(schemaKey)
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

    val thirdSucceeded = response3 must beEqualTo(correctSchema)
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

    implicit val cache: CreateResolverCache[StaticLookup] = ResolverSpecHelpers.staticResolverCache
    implicit val clock: Clock[StaticLookup]               = ResolverSpecHelpers.staticClock
    implicit val registryLookup: RegistryLookup[StaticLookup] =
      ResolverSpecHelpers.getLookup(responses, Nil)

    val result = for {
      resolver <-
        Resolver
          .init[StaticLookup](10, Some(1.second), Repos.httpRep)
      _      <- resolver.lookupSchema(schemaKey)
      _      <- StaticLookup.addTime(2.seconds)
      _      <- resolver.lookupSchema(schemaKey)
      _      <- StaticLookup.addTime(2.seconds)
      _      <- resolver.lookupSchema(schemaKey)
      _      <- StaticLookup.addTime(2.seconds)
      result <- resolver.lookupSchema(schemaKey) // ... but don't try to overwrite it
    } yield result

    val (state, response) = result.run(ResolverSpecHelpers.RegistryState.init).value

    // Final response must not overwrite a successful one
    val finalResult = response must beRight(correctSchema)

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
    val httpRep1 = Repos.httpRep.copy(config = Repos.httpRep.config.copy(name = "Mock Repo 1"))
    val httpRep2 = Repos.httpRep.copy(config = Repos.httpRep.config.copy(name = "Mock Repo 2"))

    implicit val cache: CreateResolverCache[StaticLookup] = ResolverSpecHelpers.staticResolverCache
    implicit val clock: Clock[StaticLookup]               = ResolverSpecHelpers.staticClock
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
      _        <- resolver.lookupSchema(schemaKey)
      _        <- StaticLookup.addTime(2000.millis)
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
      .map { parsed =>
        parsed must beRight[Resolver[IO]].like { case resolver =>
          resolver.cache.flatMap(_.ttl) must beSome(10.seconds) and
            (resolver.repos must contain(SpecHelpers.IgluCentral, Repos.three))
        }
      }
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
      case (Right(one), Right(two)) => one shouldNotEqual two
      case _                        => ko("Unexpected result for two consequent listSchemas")
    }
  }

  def e12 = {
    val IgluCentralServer = Registry.Http(
      Registry.Config("Iglu Central  EU1", 0, List("com.snowplowanalytics")),
      Registry
        .HttpConnection(URI.create("https://com-iglucentral-eu1-prod.iglu.snplow.net/api"), None)
    )

    val schema100 = SchemaKey(
      "com.snowplowanalytics.snowplow",
      "link_click",
      "jsonschema",
      SchemaVer.Full(1, 0, 0)
    )
    val schema101 = SchemaKey(
      "com.snowplowanalytics.snowplow",
      "link_click",
      "jsonschema",
      SchemaVer.Full(1, 0, 1)
    )

    val resolverRef: Resolver[Id] = Resolver.init[Id](10, None, IgluCentralServer)
    val resolver = // resolverRef.map(res =>
      new Resolver(
        resolverRef.repos,
        resolverRef.cache.flatMap { c =>
          c.putSchemaList(
            "com.snowplowanalytics.snowplow",
            "link_click",
            1,
            SchemaList.parseUnsafe(List(schema100)).asRight
          )
          c.some
        }
      )
    // )

    val resultOne = resolver.listSchemasLike(schema100)
    val resultTwo = resolver.listSchemasLike(schema101)

    resultOne must beRight(SchemaList.parseUnsafe(List(schema100)))
    resultTwo.map(s => SchemaList.parseUnsafe(s.schemas.take(2))) must beRight(
      SchemaList.parseUnsafe(List(schema100, schema101))
    )
  }

  def e13 = {
    val IgluCentralServer = Registry.Http(
      Registry.Config("Iglu Central  EU1", 10, List("com.snowplowanalytics")),
      Registry
        .HttpConnection(URI.create("https://com-iglucentral-eu1-prod.iglu.snplow.net/api"), None)
    )

    val schema100 = SchemaKey(
      "com.snowplowanalytics.snowplow",
      "client_session",
      "jsonschema",
      SchemaVer.Full(1, 0, 0)
    )
    val schema101 = schema100.copy(version = SchemaVer.Full(1, 0, 1))
    val schema102 = schema100.copy(version = SchemaVer.Full(1, 0, 2))

    val resolver: Resolver[Id] =
      Resolver.init[Id](10, None, IgluCentralServer, SpecHelpers.EmbeddedTest)

    // Embedded registry only contains 1-0-0 and 1-0-1 versions of client_session schema, while Iglu Central
    // contains 1-0-0, 1-0-1 and 1-0-2 versions. When listSchemasLike is called with 1-0-2, even though Iglu Central
    // has lower priority than embedded registry, we expect 1-0-2 to be returned as well in the result list.
    val result = resolver.listSchemasLike(schema102)

    result must beRight(SchemaList.parseUnsafe(List(schema100, schema101, schema102)))
  }

  def e14 = {
    val resolver: Resolver[Id] =
      Resolver
        .init[Id](0, None, SpecHelpers.IgluCentral, SpecHelpers.IgluCentralMirror, Repos.custom)

    val resolutionError = ResolutionError(
      SortedMap(
        SpecHelpers.IgluCentral.config.name -> LookupHistory(
          Set(RegistryError.NotFound),
          1,
          Instant.now()
        ),
        SpecHelpers.IgluCentralMirror.config.name -> LookupHistory(
          Set(RegistryError.NotFound),
          1,
          Instant.now()
        ),
        Repos.custom.config.name -> LookupHistory(Set(RegistryError.NotFound), 1, Instant.now())
      )
    )

    resolver.isNotFound(resolutionError) should beTrue
  }

  def e15 = {
    val resolver: Resolver[Id] =
      Resolver
        .init[Id](0, None, SpecHelpers.IgluCentral, SpecHelpers.IgluCentralMirror, Repos.custom)

    val resolutionError = ResolutionError(
      SortedMap(
        SpecHelpers.IgluCentral.config.name -> LookupHistory(
          Set(RegistryError.RepoFailure("Problem")),
          1,
          Instant.now()
        ),
        SpecHelpers.IgluCentralMirror.config.name -> LookupHistory(
          Set(RegistryError.NotFound),
          1,
          Instant.now()
        ),
        Repos.custom.config.name -> LookupHistory(Set(RegistryError.NotFound), 1, Instant.now())
      )
    )

    resolver.isNotFound(resolutionError) should beTrue
  }

  def e16 = {
    val resolver: Resolver[Id] =
      Resolver
        .init[Id](0, None, SpecHelpers.IgluCentral, SpecHelpers.IgluCentralMirror, Repos.custom)

    val resolutionError = ResolutionError(
      SortedMap(
        SpecHelpers.IgluCentral.config.name -> LookupHistory(
          Set(RegistryError.NotFound),
          1,
          Instant.now()
        ),
        SpecHelpers.IgluCentralMirror.config.name -> LookupHistory(
          Set(RegistryError.NotFound),
          1,
          Instant.now()
        ),
        Repos.custom.config.name -> LookupHistory(
          Set(RegistryError.RepoFailure("Something went wrong")),
          1,
          Instant.now()
        )
      )
    )

    resolver.isNotFound(resolutionError) should beFalse
  }

  def e17 = {
    val resolver: Resolver[Id] =
      Resolver
        .init[Id](0, None, SpecHelpers.IgluCentral, SpecHelpers.IgluCentralMirror, Repos.custom)

    val resolutionError = ResolutionError(
      SortedMap(
        SpecHelpers.IgluCentral.config.name -> LookupHistory(
          Set(RegistryError.NotFound),
          1,
          Instant.now()
        ),
        SpecHelpers.IgluCentralMirror.config.name -> LookupHistory(
          Set(RegistryError.NotFound),
          1,
          Instant.now()
        ),
        Repos.custom.config.name -> LookupHistory(
          Set(RegistryError.ClientFailure("402")),
          1,
          Instant.now()
        )
      )
    )

    resolver.isNotFound(resolutionError) should beTrue
  }

  def e18 = {
    val resolver: Resolver[Id] =
      Resolver.init[Id](0, None, SpecHelpers.IgluCentral, SpecHelpers.IgluCentralMirror)

    val resolutionError = ResolutionError(
      SortedMap(
        SpecHelpers.IgluCentral.config.name -> LookupHistory(
          Set(RegistryError.RepoFailure("Problem")),
          1,
          Instant.now()
        ),
        SpecHelpers.IgluCentralMirror.config.name -> LookupHistory(
          Set(RegistryError.NotFound),
          1,
          Instant.now()
        )
      )
    )

    resolver.isNotFound(resolutionError) should beTrue
  }

  def e19 = {
    val resolver: Resolver[Id] =
      Resolver.init[Id](0, None, SpecHelpers.IgluCentral, SpecHelpers.IgluCentralMirror)

    val resolutionError = ResolutionError(
      SortedMap(
        SpecHelpers.IgluCentral.config.name -> LookupHistory(
          Set(RegistryError.RepoFailure("Problem")),
          1,
          Instant.now()
        ),
        SpecHelpers.IgluCentralMirror.config.name -> LookupHistory(
          Set(RegistryError.RepoFailure("Network issue")),
          1,
          Instant.now()
        )
      )
    )

    resolver.isNotFound(resolutionError) should beFalse
  }

  def e20 = {
    val resolver: Resolver[Id] =
      Resolver.init[Id](0, None, SpecHelpers.IgluCentral, SpecHelpers.IgluCentralMirror)

    val resolutionError = ResolutionError(
      SortedMap(
        SpecHelpers.IgluCentral.config.name -> LookupHistory(
          Set(RegistryError.RepoFailure("Problem")),
          1,
          Instant.now()
        ),
        SpecHelpers.IgluCentralMirror.config.name -> LookupHistory(
          Set(RegistryError.ClientFailure("403 Forbidden")),
          1,
          Instant.now()
        )
      )
    )

    resolver.isNotFound(resolutionError) should beTrue
  }

  def e21 = {
    val resolver: Resolver[Id] =
      Resolver.init[Id](0, None, Repos.custom)

    val resolutionError = ResolutionError(
      SortedMap(
        Repos.custom.config.name -> LookupHistory(Set(RegistryError.NotFound), 1, Instant.now())
      )
    )

    resolver.isNotFound(resolutionError) should beTrue
  }

  def e22 = {
    val resolver: Resolver[Id] =
      Resolver.init[Id](0, None, Repos.custom)

    val resolutionError = ResolutionError(
      SortedMap(
        Repos.custom.config.name -> LookupHistory(
          Set(RegistryError.RepoFailure("Boom")),
          1,
          Instant.now()
        )
      )
    )

    resolver.isNotFound(resolutionError) should beFalse
  }

  def e23 = {
    val resolver: Resolver[Id] =
      Resolver.init[Id](0, None, Repos.custom)

    val resolutionError = ResolutionError(
      SortedMap(
        Repos.custom.config.name -> LookupHistory(
          Set(RegistryError.ClientFailure("401")),
          1,
          Instant.now()
        )
      )
    )

    resolver.isNotFound(resolutionError) should beTrue
  }

  def e24 = {
    val resolver: Resolver[Id] =
      Resolver
        .init[Id](0, None, SpecHelpers.IgluCentral, SpecHelpers.IgluCentralMirror, Repos.custom)

    val resolutionError = ResolutionError(
      SortedMap(
        SpecHelpers.IgluCentral.config.name -> LookupHistory(
          Set(RegistryError.RepoFailure("Problem"), RegistryError.ClientFailure("Boom")),
          2,
          Instant.now()
        ),
        SpecHelpers.IgluCentralMirror.config.name -> LookupHistory(
          Set(RegistryError.RepoFailure("Problem"), RegistryError.NotFound),
          2,
          Instant.now()
        ),
        Repos.custom.config.name -> LookupHistory(Set(RegistryError.NotFound), 1, Instant.now())
      )
    )

    resolver.isNotFound(resolutionError) should beTrue
  }
}
