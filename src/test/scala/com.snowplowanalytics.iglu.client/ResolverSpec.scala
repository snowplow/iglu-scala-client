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
package com.snowplowanalytics.iglu.client

// Cats
import cats.syntax.either._
import cats.syntax.option._
import cats.syntax.validated._
import cats.syntax.parallel._
import cats.data.NonEmptyList

import cats.effect.IO

// circe
import io.circe.Json
import io.circe.syntax._
import io.circe.parser.parse
import io.circe.literal._

// Iglu Core
import com.snowplowanalytics.iglu.core.{SchemaKey, SchemaVer}

// This project
import repositories.{EmbeddedRepositoryRef, HttpRepositoryRef, RepositoryRefConfig}
import validation.ProcessingMessageMethods._
import com.snowplowanalytics.iglu.client.validation.ProcessingMessage

// Specs2
import org.specs2.Specification
import org.specs2.matcher.DataTables
import org.specs2.mock.Mockito
import org.specs2.matcher.ValidatedMatchers

object ResolverSpec {

  object Repos {

    private val embedRef: (String, Int) => EmbeddedRepositoryRef = (prefix, priority) =>
      EmbeddedRepositoryRef(
        RepositoryRefConfig("An embedded repo", priority, List(prefix)),
        "/embed-path")

    val one   = embedRef("com.acme", 0)
    val two   = embedRef("de.acompany.snowplow", 40)
    val three = embedRef("de.acompany.snowplow", 100)
  }

  def notFoundError(schemaKey: String, repos: List[String]): String =
    ProcessingMessage(
      s"Could not find schema with key ${schemaKey} in any repository, tried:",
      repositories = Some(repos.asJson)).toString
}

class ResolverSpec extends Specification with DataTables with ValidatedMatchers with Mockito {
  def is = s2"""

  This is a specification to test the Resolver functionality

  our prioritizeRepos algorithm should sort repository refs in priority order  $e1
  we can construct a Resolver from a valid resolver configuration JSON  $e2
  a Resolver should report its failed lookups when a JSON Schema can't be resolved  $e3
  a Resolver should report issues with a corrupted JSON Schema  $e4
  a Resolver should report issues with invalid JSON Schema  $e5
  a Resolver should retry after non-404 errors  $e6
  a Resolver should give up after 3rd retry  $e7
  a Resolver should accumulate errors from all repositories  $e8
  a Resolver should respect cacheTtl $e9
  we can construct a Resolver from a valid resolver 1-0-2 configuration JSON  $e10
  """

  import ResolverSpec._

  def e1 = {
    val resolver =
      Resolver[IO](cacheSize = 10, SpecHelpers.IgluCentral, Repos.one, Repos.two, Repos.three)
    val schemaKey =
      SchemaKey("de.acompany.snowplow", "mobile_context", "jsonschema", SchemaVer.Full(1, 0, 0))

    resolver
      .map { resolver =>
        Resolver.prioritizeRepos(schemaKey, resolver.allRepos) must_== List(
          Repos.two,
          Repos.three,
          Bootstrap.Repo,
          Repos.one,
          SpecHelpers.IgluCentral)
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
    val expected = NonEmptyList.one(
      notFoundError(
        "iglu:com.acme.icarus/wing/jsonschema/1-0-0",
        List("Iglu Test Embedded [embedded]", "Iglu Client Embedded [embedded]")
      )
    )

    SpecHelpers.TestResolver
      .flatMap(resolver => resolver.lookupSchema(schemaKey))
      .map(result => result.leftMap(_.map(_.toString)) must beLeft(expected))
      .unsafeRunSync()
  }

  def e4 = {

    val schemaKey =
      SchemaKey(
        "com.snowplowanalytics.iglu-test",
        "corrupted_schema",
        "jsonschema",
        SchemaVer.Full(1, 0, 0))
    val expected = NonEmptyList.of(
      notFoundError(
        "iglu:com.snowplowanalytics.iglu-test/corrupted_schema/jsonschema/1-0-0",
        List("Iglu Client Embedded [embedded]", "Iglu Test Embedded [embedded]")
      ),
      ("Problem parsing /iglu-test-embedded/schemas/com.snowplowanalytics.iglu-test/corrupted_schema/jsonschema/1-0-0 as JSON in embedded Iglu repository Iglu Test Embedded: exhausted inpu" +
        "t").toProcessingMessage.toString
    )

    SpecHelpers.TestResolver
      .flatMap(resolver => resolver.lookupSchema(schemaKey))
      .map(result => result.leftMap(_.map(_.toString)) must beLeft(expected))
      .unsafeRunSync()
  }

  def e5 = {
    val schemaKey =
      SchemaKey(
        "com.snowplowanalytics.iglu-test",
        "invalid_schema",
        "jsonschema",
        SchemaVer.Full(1, 0, 0))
    val expected = NonEmptyList.of(
      notFoundError(
        "iglu:com.snowplowanalytics.iglu-test/invalid_schema/jsonschema/1-0-0",
        List("Iglu Client Embedded [embedded]", "Iglu Test Embedded [embedded]")
      ),
      "array must have at least one element".toProcessingMessage.toString
    )

    SpecHelpers.TestResolver
      .flatMap(resolver => resolver.lookupSchema(schemaKey))
      .map(result => result.leftMap(_.length))
      .map(_ must beLeft(2))
      .unsafeRunSync()
  }

  def e6 = {
    val schemaKey =
      SchemaKey(
        "com.snowplowanalytics.iglu-test",
        "mock_schema",
        "jsonschema",
        SchemaVer.Full(1, 0, 0))
    val timeoutError = IO.pure(ProcessingMessage("Timeout exception").asLeft[Option[Json]])
    val correctSchema =
      json"""{
       	"$$schema": "http://iglucentral.com/schemas/com.snowplowanalytics.self-desc/schema/jsonschema/1-0-0#",
       	"self": {
       		"vendor": "com.snowplowanalytics.iglu-test",
       		"name": "mock_schema",
       		"format": "jsonschema",
       		"version": "1-0-0"
       	}
       }""".some.asRight

    // Mocking repository
    val httpRep = mock[HttpRepositoryRef]
    httpRep.vendorMatched(any[SchemaKey]) returns true
    httpRep.config returns RepositoryRefConfig(
      "Mock Repo",
      1,
      List("com.snowplowanalytics.iglu-test"))
    httpRep.classPriority returns 1

    // Stubbing
    httpRep.lookupSchema[IO](schemaKey) returns timeoutError thenReturns IO.pure(correctSchema)

    val resolver = Resolver[IO](10, httpRep)

    val test1 = resolver
      .flatMap(resolver => resolver.lookupSchema(schemaKey))
      .map(_ must beLeft)

    val test2 = resolver
      .flatMap(_.lookupSchema(schemaKey))
      .map(_ must beEqualTo(correctSchema.map(_.get)))

    (test1, test2).parMapN(_ and _).unsafeRunSync()
  }

  def e7 = {
    val schemaKey =
      SchemaKey(
        "com.snowplowanalytics.iglu-test",
        "future_schema",
        "jsonschema",
        SchemaVer.Full(1, 0, 0))
    val timeout      = ProcessingMessage("Timeout exception")
    val timeoutError = IO.pure(timeout.asLeft[Option[Json]])
    val correctSchema =
      IO.pure(json"""{
       	"$$schema": "http://iglucentral.com/schemas/com.snowplowanalytics.self-desc/schema/jsonschema/1-0-0#",
       	"self": {
       		"vendor": "com.snowplowanalytics.iglu-test",
       		"name": "future_schema",
       		"format": "jsonschema",
       		"version": "1-0-0"
       	}
       }""".some.asRight)

    // Mocking repository
    val httpRep = mock[HttpRepositoryRef]
    httpRep.vendorMatched(any[SchemaKey]) returns true
    httpRep.config returns RepositoryRefConfig(
      "Mock Repo",
      1,
      List("com.snowplowanalytics.iglu-test"))
    httpRep.classPriority returns 1

    // Stubbing (return error three times in a row)
    httpRep.lookupSchema[IO](schemaKey) returns timeoutError thenReturns timeoutError thenReturns timeoutError thenReturns correctSchema

    val result = for {
      resolver <- Resolver[IO](10, httpRep)
      _        <- resolver.lookupSchema(schemaKey)
      _        <- resolver.lookupSchema(schemaKey)
      _        <- resolver.lookupSchema(schemaKey)
      _        <- resolver.lookupSchema(schemaKey) // this and subsequent return error
      result   <- resolver.lookupSchema(schemaKey)

      matcher = result must beLeft.like {
        case error =>
          error.toList.map(_.toString) must contain(
            notFoundError(
              schemaKey.toSchemaUri,
              List("Iglu Client Embedded [embedded]", "Mock Repo [null]")),
            timeout.toString
          )
      }
    } yield matcher and (there was 3.times(httpRep).lookupSchema[IO](schemaKey))

    result.unsafeRunSync()
  }

  def e8 = {
    val schemaKey =
      SchemaKey(
        "com.snowplowanalytics.iglu-test",
        "future_schema",
        "jsonschema",
        SchemaVer.Full(1, 0, 0))
    val error1 = ProcessingMessage("Timeout exception")
    val error2 = ProcessingMessage("Network exception")
    val error3 = ProcessingMessage("Unknown exception")
    val error4 = ProcessingMessage("Server segfault")

    // Mocking repositories
    val httpRep1 = mock[HttpRepositoryRef]
    httpRep1.vendorMatched(any[SchemaKey]) returns true
    httpRep1.config returns RepositoryRefConfig(
      "Mock Repo 1",
      1,
      List("com.snowplowanalytics.iglu-test"))
    httpRep1.classPriority returns 1
    val httpRep2 = mock[HttpRepositoryRef]
    httpRep2.vendorMatched(any[SchemaKey]) returns true
    httpRep2.config returns RepositoryRefConfig(
      "Mock Repo 2",
      2,
      List("com.snowplowanalytics.iglu-test"))
    httpRep2.classPriority returns 1

    // Stubbing
    httpRep1.lookupSchema[IO](schemaKey) returns IO.pure(error1.asLeft) thenReturns IO.pure(
      error2.asLeft)
    httpRep2.lookupSchema[IO](schemaKey) returns IO.pure(error3.asLeft) thenReturns IO.pure(
      error4.asLeft)

    val result = for {
      resolver <- Resolver[IO](10, httpRep1, httpRep2)
      _        <- resolver.lookupSchema(schemaKey)
      result   <- resolver.lookupSchema(schemaKey)

      matcher = result must beLeft.like {
        case error =>
          error.toList.map(_.toString) must contain(
            notFoundError(
              schemaKey.toSchemaUri,
              List("Iglu Client Embedded [embedded]", "Mock Repo 2 [null]", "Mock Repo 1 [null]")),
            error1.toString,
            error2.toString,
            error3.toString,
            error4.toString
          )

      }
    } yield matcher and (there was 2.times(httpRep1).lookupSchema[IO](schemaKey))

    result.unsafeRunSync()
  }

  def e9 = {
    val schemaKey =
      SchemaKey(
        "com.snowplowanalytics.iglu-test",
        "future_schema",
        "jsonschema",
        SchemaVer.Full(1, 0, 0))
    val timeout      = ProcessingMessage("Timeout exception")
    val timeoutError = timeout.asLeft[Option[Json]]
    val correctSchema =
      json"""{
       	"$$schema": "http://iglucentral.com/schemas/com.snowplowanalytics.self-desc/schema/jsonschema/1-0-0#",
       	"self": {
       		"vendor": "com.snowplowanalytics.iglu-test",
       		"name": "future_schema",
       		"format": "jsonschema",
       		"version": "1-0-0"
        }
       }""".some.asRight

    def notFound = IO.pure(Option.empty[Json].asRight) // 404

    // Mocking repository
    val httpRep = mock[HttpRepositoryRef]
    httpRep.vendorMatched(any[SchemaKey]) returns true
    httpRep.config returns RepositoryRefConfig(
      "Mock Repo",
      1,
      List("com.snowplowanalytics.iglu-test"))
    httpRep.classPriority returns 1

    // Stubbing
    httpRep.lookupSchema[IO](schemaKey) returns notFound thenReturns IO.pure(correctSchema)

    val result = for {
      resolver          <- SchemaCache[IO](10, Some(3)).map(cache => Resolver(cache, List(httpRep)))
      fetchResult       <- resolver.lookupSchema(schemaKey) // not found
      immediateResult   <- resolver.lookupSchema(schemaKey) // not found, but from cache, not from RegistryRef
      _                 <- IO(Thread.sleep(3500)) // wait until cache expire
      afterCacheExpired <- resolver.lookupSchema(schemaKey) // invalidate cache, retry and succeed

      successCheck                = afterCacheExpired must beEqualTo(correctSchema.map(_.get))
      immediateCacheNotFoundCheck = immediateResult must beLeft
      cacheFirstTwoCheck          = there was 2.times(httpRep).lookupSchema[IO](schemaKey)
      fetchNotFoundCheck          = fetchResult must beLeft
    } yield
      successCheck and immediateCacheNotFoundCheck and cacheFirstTwoCheck and fetchNotFoundCheck

    result.unsafeRunSync()
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
