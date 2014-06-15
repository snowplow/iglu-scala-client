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

// Java
import java.net.URL

// Scalaz
import scalaz._
import Scalaz._

// Specs2
import org.specs2.Specification
import org.specs2.matcher.DataTables
import org.specs2.scalaz.ValidationMatchers

class HttpRepositoryRefSpec extends Specification with DataTables with ValidationMatchers { def is =

  "This is a specification to test an HTTP-based RepositoryRef"                                            ^
                                                                                                          p^
  "a JSON configuration for an HTTP-based RepositoryRef should be recognized as such"                      ! e1^
  "a JSON configuration can be used to construct an HTTP-based RepositoryRef"                              ! e2^
  "retrieving an existent JSON Schema from an HTTP-based RepositoryRef should work"                        ! e3^
  "requesting a non-existent JSON Schema from an HTTP-based RepositoryRef should return None"              ! e4^
                                                                                                           end

  val AcmeConfig = SpecHelpers.asJValue(
       """|{
            |"name": "Acme Iglu Repo",
            |"priority": 5,
            |"vendorPrefixes": [ "com.acme" ],
            |"connection": {
              |"http": {
                |"uri": "http://iglu.acme.com"
              |}
            |}
          |}""".stripMargin.replaceAll("[\n\r]","")
      )

  def e1 = HttpRepositoryRef.isHttp(AcmeConfig) must beTrue

  def e2 = HttpRepositoryRef.parse(AcmeConfig) must beSuccessful(HttpRepositoryRef(
    config = RepositoryRefConfig("Acme Iglu Repo", 5, List("com.acme")),
    uri = new URL("http://iglu.acme.com")
    ))

  def e3 = {
    val schemaKey = SchemaKey("com.snowplowanalytics.snowplow", "link_click", "jsonschema", "1-0-0")

    val expected = SpecHelpers.asJsonNode(
       """|{
            |"$schema": "http://iglucentral.com/schemas/com.snowplowanalytics.self-desc/schema/jsonschema/1-0-0#",
            |"description": "Schema for a link click event",
            |"self": {
              |"vendor": "com.snowplowanalytics.snowplow",
              |"name": "link_click",
              |"format": "jsonschema",
              |"version": "1-0-0"
            |},
            |"type": "object",
            |"properties": {
              |"elementId": {
                |"type": "string"
              |},
              |"elementClasses": {
                |"type": "array",
                |"items": {
                  |"type": "string"
                |}
              |},
              |"elementTarget": {
                |"type": "string"
              |},
              |"targetUrl": {
                |"type": "string",
                |"minLength": 1
              |}
            |},
            |"required": ["targetUrl"],
            |"additionalProperties": false
          |}""".stripMargin.replaceAll("[\n\r]","")
    )

    val actual = SpecHelpers.IgluCentral.lookupSchema(schemaKey)
    actual.map(_.map(_.toString)) must beSuccessful(expected.toString.some)
  }

  def e4 = {
    val schemaKey = SchemaKey("de.ersatz.n-a", "null", "jsonschema", "1-0-0")
    SpecHelpers.IgluCentral.lookupSchema(schemaKey) must beSuccessful(None)
  }

}
