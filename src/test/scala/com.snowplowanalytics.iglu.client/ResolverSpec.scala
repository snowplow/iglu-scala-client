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

// Java
import java.net.URL

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

class ResolverSpec extends Specification with DataTables with ValidationMatchers { def is =

  "This is a specification to test the Resolver functionality"                       ^
                                                                                    p^
  "our prioritizeRepos algorithm should sort repository refs in priority order"      ! e1^
  "we can construct a Resolver from a valid resolver configuration JSON"             ! e2^
  "a Resolver should report its failed lookups when a JSON Schema can't be resolved" ! e3^
  "a Resolver should report issues with a corrupted JSON Schema"                     ! e4^
                                                                                     end

  import ResolverSpec._

  def e1 = {
    val resolver = Resolver(cacheSize = 0, SpecHelpers.IgluCentral, Repos.one, Repos.two, Repos.three)
    val schemaKey = SchemaKey("de.acompany.snowplow", "mobile_context", "jsonschema", "1-0-0")

    resolver.prioritizeRepos(schemaKey) must_== List(Repos.two, Repos.three, Bootstrap.Repo, Repos.one, SpecHelpers.IgluCentral)
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

}
