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
package com.snowplowanalytics.iglu.client.resolver.registries

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

class EmbeddedSpec extends Specification with DataTables with ValidatedMatchers {
  def is = s2"""

  This is a specification to test an embedded RepositoryRef

  a JSON configuration can be used to construct an embedded RepositoryRef  $e2
  retrieving an existent JSON Schema from an embedded RepositoryRef should work  $e3
  requesting a non-existent JSON Schema from an embedded RepositoryRef should return None  $e4
  requesting a corrupted JSON Schema from an embedded RepositoryRef should return an appropriate Failure  $e5
  """

  val AcmeConfig =
    json"""{
          "name": "Acme Embedded",
          "priority": 100,
          "vendorPrefixes": [ "uk.co.acme", "de.acme" ],
          "connection": {
            "embedded": {
              "path": "/acme-embedded-new"
            }
          }
        }"""

  def e2 = {
    val expected = Registry.Embedded(
      config = Registry.Config("Acme Embedded", 100, List("uk.co.acme", "de.acme")),
      path = "/acme-embedded-new"
    )
    Registry.parse(AcmeConfig) must beRight(expected)
  }

  def e3 = {
    val schemaKey =
      SchemaKey(
        "com.snowplowanalytics.iglu-test",
        "stock-item",
        "jsonschema",
        SchemaVer.Full(1, 0, 0)
      )
    val expected =
      json"""{
        "$$schema":"http://iglucentral.com/schemas/com.snowplowanalytics.self-desc/schema/jsonschema/1-0-0#",
        "description":"Test schema",
        "self":{
          "vendor":"com.snowplowanalytics.iglu-test",
          "name":"stock-item",
          "format":"jsonschema",
          "version":"1-0-0"
        },
        "type":"object",
        "properties":{
          "id":{
            "type":"string"
          },
          "name":{
            "type":"string"
          },
          "price":{
            "type":"number"
          }
        },
        "required":["id","name","price"],
        "additionalProperties":false
      }"""

    SpecHelpers.EmbeddedTest
      .lookupSchema[IO](schemaKey)
      .unsafeRunSync() must beRight(expected)
  }

  def e4 = {
    val schemaKey = SchemaKey("com.acme.n-a", "null", "jsonschema", SchemaVer.Full(1, 0, 0))
    SpecHelpers.EmbeddedTest
      .lookupSchema[IO](schemaKey)
      .unsafeRunSync() must beLeft(RegistryError.NotFound: RegistryError)
  }

  def e5 = {
    val schemaKey =
      SchemaKey(
        "com.snowplowanalytics.iglu-test",
        "corrupted_schema",
        "jsonschema",
        SchemaVer.Full(1, 0, 0)
      )
    SpecHelpers.EmbeddedTest
      .lookupSchema[IO](schemaKey)
      .unsafeRunSync() must beLeft
  }

}
