/*
 * Copyright (c) 2014 Snowplow Analytics Ltd. All rights reserved.
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

// JSON Schema
import com.github.fge.jsonschema.core.report.{
  ProcessingMessage,
  LogLevel
}

// Scalaz
import scalaz._
import Scalaz._

// json4s
import org.json4s._
import org.json4s.JsonDSL._
import org.json4s.jackson.JsonMethods._

import com.fasterxml.jackson.databind.JsonNode

// This project
import repositories.{
  EmbeddedRepositoryRef,
  HttpRepositoryRef,
  RepositoryRefConfig
}
import validation.ProcessingMessageMethods._

// Specs2
import org.specs2.Specification
import org.specs2.matcher.DataTables
import org.specs2.mock.Mockito
import org.specs2.scalaz.ValidationMatchers

object ResolverSpec {

  object Repos { 

    private val embedRef: (String, Int) => EmbeddedRepositoryRef = (prefix, priority) =>
      EmbeddedRepositoryRef(RepositoryRefConfig("An embedded repo", priority, List(prefix)), "/embed-path")

    val one   = embedRef("com.acme", 0)
    val two   = embedRef("de.acompany.snowplow", 40)
    val three = embedRef("de.acompany.snowplow", 100)
  }

  def notFoundError(schemaKey: String, repos: List[String]): String =
    new ProcessingMessage()
      .setLogLevel(LogLevel.ERROR)
      .setMessage(s"Could not find schema with key ${schemaKey} in any repository, tried:")
      .put("repositories", asJsonNode(repos))
      .toString
}

class ResolverSpec extends Specification with DataTables with ValidationMatchers with Mockito { def is =

  "This is a specification to test the Resolver functionality"                       ^
                                                                                    p^
  "our prioritizeRepos algorithm should sort repository refs in priority order"      ! e1^
  "we can construct a Resolver from a valid resolver configuration JSON"             ! e2^
  "a Resolver should report its failed lookups when a JSON Schema can't be resolved" ! e3^
  "a Resolver should report issues with a corrupted JSON Schema"                     ! e4^
  "a Resolver should report issues with invalid JSON Schema"                         ! e5^
  "a Resolver should retry after non-404 errors"                                     ! e6^
  "a Resolver should give up after 3rd retry"                                        ! e7^
  "a Resolver should accumulate errors from all repositories"                        ! e8^
                                                                                     end

  import ResolverSpec._

  def e1 = {
    val resolver = Resolver(cacheSize = 0, SpecHelpers.IgluCentral, Repos.one, Repos.two, Repos.three)
    val schemaKey = SchemaKey("de.acompany.snowplow", "mobile_context", "jsonschema", "1-0-0")

    Resolver.prioritizeRepos(schemaKey, resolver.allRepos) must_== List(Repos.two, Repos.three, Bootstrap.Repo, Repos.one, SpecHelpers.IgluCentral)
  }

  def e2 = {

    val config = 
      s"""|{
            |"schema": "iglu:com.snowplowanalytics.iglu/resolver-config/jsonschema/1-0-0",
            |"data": {
              |"cacheSize": 500,
              |"repositories": [
                |{
                  |"name": "Iglu Central",
                  |"priority": 0,
                  |"vendorPrefixes": [ "com.snowplowanalytics" ],
                  |"connection": {
                    |"http": {
                      |"uri": "http://iglucentral.com"
                    |}
                  |}
                |}, {                
                  |"name": "An embedded repo",
                  |"priority": 100,
                  |"vendorPrefixes": [ "de.acompany.snowplow" ],
                  |"connection": {
                    |"embedded": {
                      |"path": "/embed-path"
                    |}
                  |}
                |}
              |]
            |}
          |}""".stripMargin.replaceAll("[\n\r]","")

    val expected = Resolver(cacheSize = 500, SpecHelpers.IgluCentral, Repos.three)

    Resolver.parse(SpecHelpers.asJsonNode(config)) must beSuccessful(expected)
  }

  def e3 = {

    val schemaKey = SchemaKey("com.acme.icarus", "wing", "jsonschema", "1-0-0")
    val expected = NonEmptyList(
      notFoundError(
        "iglu:com.acme.icarus/wing/jsonschema/1-0-0",
        List("Iglu Test Embedded [embedded]", "Iglu Client Embedded [embedded]")
      )
    )

    val actual = SpecHelpers.TestResolver.lookupSchema(schemaKey)
    actual.leftMap(_.map(_.toString)) must beFailing(expected)
  }

  def e4 = {

    val schemaKey = SchemaKey("com.snowplowanalytics.iglu-test", "corrupted_schema", "jsonschema", "1-0-0")
    val expected = NonEmptyList(
      notFoundError(
        "iglu:com.snowplowanalytics.iglu-test/corrupted_schema/jsonschema/1-0-0",
        List("Iglu Client Embedded [embedded]", "Iglu Test Embedded [embedded]")
      ),
      "Problem parsing /iglu-test-embedded/schemas/com.snowplowanalytics.iglu-test/corrupted_schema/jsonschema/1-0-0 as JSON in embedded Iglu repository Iglu Test Embedded: Unexpected end-of-input within/between OBJECT entries at [Source: java.io.BufferedInputStream@xxxxxx; line: 10, column: 316]".toProcessingMessage.toString
    )

    val actual = SpecHelpers.TestResolver.lookupSchema(schemaKey)
    actual.leftMap(_.map(_.toString)) must beFailing(expected)
  }

  def e5 = {
    val schemaKey = SchemaKey("com.snowplowanalytics.iglu-test", "invalid_schema", "jsonschema", "1-0-0")
    val expected = NonEmptyList(
      notFoundError(
        "iglu:com.snowplowanalytics.iglu-test/invalid_schema/jsonschema/1-0-0",
        List("Iglu Client Embedded [embedded]", "Iglu Test Embedded [embedded]")
      ),
      "array must have at least one element".toProcessingMessage
        .put("domain", "syntax")
        .put("schema", asJsonNode(parse("""{"loadingURI":"#","pointer":""}""")))
        .put("keyword", "required")
        .toString
    )

    val actual = SpecHelpers.TestResolver.lookupSchema(schemaKey)
    actual.leftMap(_.map(_.toString)) must beFailing(expected)
  }

  def e6 = {
    val schemaKey = SchemaKey("com.snowplowanalytics.iglu-test", "mock_schema", "jsonschema", "1-0-0")
    val timeoutError = new ProcessingMessage()
        .setLogLevel(LogLevel.ERROR)
        .setMessage("Timeout exception")
        .failure[Option[JsonNode]]
    val correctSchema = asJsonNode(parse("""|{
       |	"$schema": "http://iglucentral.com/schemas/com.snowplowanalytics.self-desc/schema/jsonschema/1-0-0#",
       |	"self": {
       |		"vendor": "com.snowplowanalytics.iglu-test",
       |		"name": "mock_schema",
       |		"format": "jsonschema",
       |		"version": "1-0-0"
       |	}
       |}""".stripMargin)).some.success

    // Mocking repository
    val httpRep = mock[HttpRepositoryRef]
    httpRep.vendorMatched(any[SchemaKey]) returns true
    httpRep.config returns RepositoryRefConfig("Mock Repo", 1, List("com.snowplowanalytics.iglu-test"))
    httpRep.classPriority returns 1

    // Stubbing
    httpRep.lookupSchema(schemaKey) returns timeoutError thenReturns correctSchema

    val resolver = Resolver(10, List(httpRep))
    resolver.lookupSchema(schemaKey) must beFailing and(
      resolver.lookupSchema(schemaKey) must beEqualTo(correctSchema.map(_.get))
    )
  }

  def e7 = {
    val schemaKey = SchemaKey("com.snowplowanalytics.iglu-test", "future_schema", "jsonschema", "1-0-0")
    val timeout = new ProcessingMessage().setLogLevel(LogLevel.ERROR).setMessage("Timeout exception")
    val timeoutError = timeout.failure[Option[JsonNode]]
    val correctSchema = asJsonNode(parse("""|{
       |	"$schema": "http://iglucentral.com/schemas/com.snowplowanalytics.self-desc/schema/jsonschema/1-0-0#",
       |	"self": {
       |		"vendor": "com.snowplowanalytics.iglu-test",
       |		"name": "future_schema",
       |		"format": "jsonschema",
       |		"version": "1-0-0"
       |	}
       |}""".stripMargin)).some.success

    // Mocking repository
    val httpRep = mock[HttpRepositoryRef]
    httpRep.vendorMatched(any[SchemaKey]) returns true
    httpRep.config returns RepositoryRefConfig("Mock Repo", 1, List("com.snowplowanalytics.iglu-test"))
    httpRep.classPriority returns 1

    // Stubbing (return error three times in a row)
    httpRep.lookupSchema(schemaKey) returns timeoutError thenReturns timeoutError thenReturns timeoutError thenReturns correctSchema

    val resolver = Resolver(10, List(httpRep))
    resolver.lookupSchema(schemaKey)
    resolver.lookupSchema(schemaKey)
    resolver.lookupSchema(schemaKey)
    resolver.lookupSchema(schemaKey) // this and subsequent return error

    resolver.lookupSchema(schemaKey) must beFailing.like {
      case error => error.toList.map(_.toString) must contain(
        notFoundError(schemaKey.toString, List("Iglu Client Embedded [embedded]","Mock Repo [null]")),
        timeout.toString
      )
    } and(there was 3.times(httpRep).lookupSchema(schemaKey))
  }

  def e8 = {
    val schemaKey = SchemaKey("com.snowplowanalytics.iglu-test", "future_schema", "jsonschema", "1-0-0")
    val error1 = new ProcessingMessage().setLogLevel(LogLevel.ERROR).setMessage("Timeout exception")
    val error2 = new ProcessingMessage().setLogLevel(LogLevel.ERROR).setMessage("Network exception")
    val error3 = new ProcessingMessage().setLogLevel(LogLevel.ERROR).setMessage("Unknown exception")
    val error4 = new ProcessingMessage().setLogLevel(LogLevel.ERROR).setMessage("Server segfault")

    // Mocking repositories
    val httpRep1 = mock[HttpRepositoryRef]
    httpRep1.vendorMatched(any[SchemaKey]) returns true
    httpRep1.config returns RepositoryRefConfig("Mock Repo 1", 1, List("com.snowplowanalytics.iglu-test"))
    httpRep1.classPriority returns 1
    val httpRep2 = mock[HttpRepositoryRef]
    httpRep2.vendorMatched(any[SchemaKey]) returns true
    httpRep2.config returns RepositoryRefConfig("Mock Repo 2", 2, List("com.snowplowanalytics.iglu-test"))
    httpRep2.classPriority returns 1

    // Stubbing
    httpRep1.lookupSchema(schemaKey) returns error1.failure[Option[JsonNode]] thenReturns error2.failure[Option[JsonNode]]
    httpRep2.lookupSchema(schemaKey) returns error3.failure[Option[JsonNode]] thenReturns error4.failure[Option[JsonNode]]

    val resolver = Resolver(10, List(httpRep1, httpRep2))
    resolver.lookupSchema(schemaKey)

    resolver.lookupSchema(schemaKey) must beFailing.like {
      case error => error.toList.map(_.toString) must contain(
        notFoundError(schemaKey.toString, List("Iglu Client Embedded [embedded]","Mock Repo 2 [null]","Mock Repo 1 [null]")),
        error1.toString, error2.toString, error3.toString, error4.toString
      )
    } and(there was 2.times(httpRep1).lookupSchema(schemaKey))
  }
}
