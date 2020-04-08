/*
 * Copyright (c) 2014-2019 Snowplow Analytics Ltd. All rights reserved.
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
package com.snowplowanalytics.iglu.client.resolver.registries

// Java
import java.net.URI

// Cats
import cats.effect.IO

// circe
import io.circe.literal._

// Iglu Core
import com.snowplowanalytics.iglu.core.{SchemaKey, SchemaVer}

// This project
import com.snowplowanalytics.iglu.client.resolver.registries.RegistryLookup._

// Specs2
import org.specs2.Specification
import org.specs2.matcher.{DataTables, ValidatedMatchers}

import com.snowplowanalytics.iglu.client.SpecHelpers

class HttpSpec extends Specification with DataTables with ValidatedMatchers {
  def is = s2"""

  This is a specification to test an HTTP-based RepositoryRef

  toPath strips unnecessary trailing slash $e1
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

  def e1 = {
    val resultWithSlash = RegistryLookup.toPath(
      "http://iglucentral.com/api/",
      SchemaKey("com.snowplow", "event", "jsonschema", SchemaVer.Full(1, 0, 0)))
    val resultWithoutSlash = RegistryLookup.toPath(
      "http://iglucentral.com/api",
      SchemaKey("com.snowplow", "event", "jsonschema", SchemaVer.Full(1, 0, 0)))

    val expected = "http://iglucentral.com/api/schemas/com.snowplow/event/jsonschema/1-0-0"
    (resultWithSlash must beEqualTo(expected)) and (resultWithoutSlash must beEqualTo(expected))
  }

  def e2 = {
    val expected = Registry.Http(
      Registry.Config("Acme Iglu Repo", 5, List("com.acme")),
      Registry.HttpConnection(URI.create("http://iglu.acme.com"), None)
    )
    Registry.parse(AcmeConfig) must beRight(expected)
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
      .unsafeRunSync() must beRight(expected)
  }

  def e4 = {
    val schemaKey = SchemaKey("de.ersatz.n-a", "null", "jsonschema", SchemaVer.Full(1, 0, 0))
    SpecHelpers.IgluCentral
      .lookupSchema[IO](schemaKey)
      .unsafeRunSync() must beLeft(RegistryError.NotFound: RegistryError)
  }

  def e5 = {
    val expected = Registry.Http(
      Registry.Config("Acme Secret Iglu Repo", 3, List("com.acme")),
      Registry.HttpConnection(
        URI.create("http://iglu.acme.com"),
        Some("de305d54-75b4-431b-adb2-eb6b9e546014"))
    )
    Registry.parse(AcmeConfigWithAuth) must beRight(expected)
  }
}
