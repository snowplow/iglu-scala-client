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
package repositories

// Scalaz
import scalaz._
import Scalaz._

// This project
import validation.ProcessingMessageMethods._

// Specs2
import org.specs2.Specification
import org.specs2.matcher.DataTables
import org.specs2.scalaz.ValidationMatchers

class EmbeddedRepositoryRefSpec extends Specification with DataTables with ValidationMatchers { def is = s2"""

  This is a specification to test an embedded RepositoryRef

  a JSON configuration for an embedded RepositoryRef should be recognized as such  $e1
  a JSON configuration can be used to construct an embedded RepositoryRef  $e2
  retrieving an existent JSON Schema from an embedded RepositoryRef should work  $e3
  requesting a non-existent JSON Schema from an embedded RepositoryRef should return None  $e4
  requesting a corrupted JSON Schema from an embedded RepositoryRef should return an appropriate Failure  $e5
  """

  val AcmeConfig = SpecHelpers.asJValue(
     """|{
          |"name": "Acme Embedded",
          |"priority": 100,
          |"vendorPrefixes": [ "uk.co.acme", "de.acme" ],
          |"connection": {
            |"embedded": {
              |"path": "/acme-embedded-new"
            |}
          |}
        |}""".stripMargin.replaceAll("[\n\r]","")
    )

  def e1 = EmbeddedRepositoryRef.isEmbedded(AcmeConfig) must beTrue

  def e2 = {
    val expected = EmbeddedRepositoryRef(
      config = RepositoryRefConfig("Acme Embedded", 100, List("uk.co.acme", "de.acme")),
      path = "/acme-embedded-new"
    )
    EmbeddedRepositoryRef.parse(AcmeConfig) must beSuccessful(expected)
  }

  def e3 = {
    val schemaKey = SchemaKey("com.snowplowanalytics.iglu-test", "stock-item", "jsonschema", "1-0-0")
    val expected =
      """{
        |"$schema":"http://iglucentral.com/schemas/com.snowplowanalytics.self-desc/schema/jsonschema/1-0-0#",
        |"description":"Test schema",
        |"self":{
          |"vendor":"com.snowplowanalytics.iglu-test",
          |"name":"stock-item",
          |"format":"jsonschema",
          |"version":"1-0-0"
        |},
        |"type":"object",
        |"properties":{
          |"id":{
            |"type":"string"
          |},
          |"name":{
            |"type":"string"
          |},
          |"price":{
            |"type":"number"
          |}
        |},
        |"required":["id","name","price"],
        |"additionalProperties":false
      |}""".stripMargin.replaceAll("[\n\r]","")

    SpecHelpers.EmbeddedTest.lookupSchema(schemaKey).map(_.map(_.toString)) must beSuccessful(Some(expected))
  }

  def e4 = {
    val schemaKey = SchemaKey("com.acme.n-a", "null", "jsonschema", "1-0-0")
    SpecHelpers.EmbeddedTest.lookupSchema(schemaKey) must beSuccessful(None)
  }

  def e5 = {
    val schemaKey = SchemaKey("com.snowplowanalytics.iglu-test", "corrupted_schema", "jsonschema", "1-0-0")
    val expected = "Problem parsing /iglu-test-embedded/schemas/com.snowplowanalytics.iglu-test/corrupted_schema/jsonschema/1-0-0 as JSON in embedded Iglu repository Iglu Test Embedded: Unexpected end-of-input within/between OBJECT entries at [Source: java.io.BufferedInputStream@xxxxxx; line: 10, column: 316]".toProcessingMessage.toString
    SpecHelpers.EmbeddedTest.lookupSchema(schemaKey).leftMap(_.toString) must beFailing(expected.toString)
  }

}
