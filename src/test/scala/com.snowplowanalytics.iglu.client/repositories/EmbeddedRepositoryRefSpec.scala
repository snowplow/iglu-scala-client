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

// Specs2
import org.specs2.Specification
import org.specs2.matcher.DataTables
import org.specs2.scalaz.ValidationMatchers

class EmbeddedRepositoryRefSpec extends Specification with DataTables with ValidationMatchers { def is =

  "This is a specification to test an embedded RepositoryRef"                                              ^
                                                                                                          p^
  "a JSON configuration for an embedded RepositoryRef should be recognized as such"                        ! e1^
  "a JSON configuration can be used to construct an embedded RepositoryRef"                                ! e2^
  "retrieving an existent JSON Schema from an HTTP-based RepositoryRef should work"                        ! e3^
  "requesting a non-existent JSON Schema from an embedded RepositoryRef should return None"                ! e4^
  // a corrupted JSON Schema 
                                                                                                           end

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
        |}
      |}""".stripMargin.replaceAll("[\n\r]","")

    SpecHelpers.EmbeddedTest.lookupSchema(schemaKey).map(_.map(_.toString)) must beSuccessful(Some(expected))
  }

  def e4 = {
    val schemaKey = SchemaKey("com.acme.n-a", "null", "jsonschema", "1-0-0")
    SpecHelpers.IgluCentral.lookupSchema(schemaKey) must beSuccessful(None)
  }

}
