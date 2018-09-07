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
package repositories

// Cats
import cats.syntax.option._
import cats.effect.IO

// circe
import io.circe.literal._

// Iglu Core
import com.snowplowanalytics.iglu.core.{SchemaKey, SchemaVer}

// Specs2
import org.specs2.Specification
import org.specs2.matcher.{DataTables, ValidatedMatchers}

class HttpRepositoryRefSpec extends Specification with DataTables with ValidatedMatchers {
  def is = s2"""

  This is a specification to test an HTTP-based RepositoryRef

  a JSON configuration for an HTTP-based RepositoryRef should be recognized as such  $e1
  a JSON configuration can be used to construct an HTTP-based RepositoryRef  $e2
  retrieving an existent JSON Schema from an HTTP-based RepositoryRef should work  $e3
  requesting a non-existent JSON Schema from an HTTP-based RepositoryRef should return None  $e4
  a JSON configuration can be used to construct an HttpRepositoryRef with apikey  $e5
  """

  val AcmeConfig =
    json"""{
            "name": "Acme Iglu Repo",
            "priority": 5,
            "vendorPrefixes": [ "com.acme" ],
            "connection": {
              "http": {
                "uri": "http://iglu.acme.com"
              }
            }
          }"""

  val AcmeConfigWithAuth =
    json"""{
          "name": "Acme Secret Iglu Repo",
          "priority": 3,
          "vendorPrefixes": [ "com.acme" ],
          "connection": {
            "http": {
              "apikey": "de305d54-75b4-431b-adb2-eb6b9e546014",
              "uri": "http://iglu.acme.com"
            }
          }
          }"""

  def e1 = HttpRepositoryRef.isHttp(AcmeConfig) must beTrue

  def e2 = {
    val expected = HttpRepositoryRef(
      config = RepositoryRefConfig("Acme Iglu Repo", 5, List("com.acme")),
      uri = "http://iglu.acme.com"
    )
    HttpRepositoryRef.parse(AcmeConfig) must beValid(expected)
  }

  def e3 = {
    val schemaKey = SchemaKey(
      "com.snowplowanalytics.snowplow",
      "link_click",
      "jsonschema",
      SchemaVer.Full(1, 0, 0))

    val expected =
      json"""{
            "$$schema": "http://iglucentral.com/schemas/com.snowplowanalytics.self-desc/schema/jsonschema/1-0-0#",
            "description": "Schema for a link click event",
            "self": {
              "vendor": "com.snowplowanalytics.snowplow",
              "name": "link_click",
              "format": "jsonschema",
              "version": "1-0-0"
            },
            "type": "object",
            "properties": {
              "elementId": {
                "type": "string"
              },
              "elementClasses": {
                "type": "array",
                "items": {
                  "type": "string"
                }
              },
              "elementTarget": {
                "type": "string"
              },
              "targetUrl": {
                "type": "string",
                "minLength": 1
              }
            },
            "required": ["targetUrl"],
            "additionalProperties": false
          }"""

    SpecHelpers.IgluCentral
      .lookupSchema[IO](schemaKey)
      .map(_ must beRight(expected.some))
      .unsafeRunSync()
  }

  def e4 = {
    val schemaKey = SchemaKey("de.ersatz.n-a", "null", "jsonschema", SchemaVer.Full(1, 0, 0))
    SpecHelpers.IgluCentral
      .lookupSchema[IO](schemaKey)
      .map(_ must beRight(None))
      .unsafeRunSync()
  }

  def e5 = {
    val expected = HttpRepositoryRef(
      config = RepositoryRefConfig("Acme Secret Iglu Repo", 3, List("com.acme")),
      uri = "http://iglu.acme.com",
      Some("de305d54-75b4-431b-adb2-eb6b9e546014")
    )
    HttpRepositoryRef.parse(AcmeConfigWithAuth) must beValid(expected)
  }
}
