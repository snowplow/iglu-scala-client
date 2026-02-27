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
package com.snowplowanalytics.iglu.client.resolver.registries

import cats.Id
import io.circe.Json
import io.circe.literal._

import com.snowplowanalytics.iglu.core.{SchemaKey, SchemaMap, SchemaVer, SelfDescribingSchema}
import com.snowplowanalytics.iglu.client.resolver.registries.JavaNetRegistryLookup._

import org.specs2.Specification

class InMemoryLookupSpec extends Specification {
  def is = s2"""
  InMemory registry should
    lookup a schema that exists                                    $e1
    return NotFound for a schema that does not exist               $e2
    list should return NotFound for InMemory                       $e3
    Id lookup should use InMemory registry directly                $e4
    Id lookup should use Embedded registry for Id                  $e5
  """

  val testKey: SchemaKey =
    SchemaKey("com.test", "event", "jsonschema", SchemaVer.Full(1, 0, 0))

  val testSchema: SelfDescribingSchema[Json] =
    SelfDescribingSchema(SchemaMap(testKey), json"""{"type": "object"}""")

  val inMemoryRegistry: Registry.InMemory =
    Registry.InMemory(Registry.Config("Test InMemory", 0, List("com.test")), List(testSchema))

  def e1 = {
    val result = RegistryLookup.inMemoryLookup(List(testSchema), testKey)
    result must beRight
  }

  def e2 = {
    val missingKey = SchemaKey("com.test", "missing", "jsonschema", SchemaVer.Full(1, 0, 0))
    val result     = RegistryLookup.inMemoryLookup(List(testSchema), missingKey)
    result must beLeft(RegistryError.NotFound: RegistryError)
  }

  def e3 = {
    val result: Id[Either[RegistryError, _]] =
      idLookupInstance.list(inMemoryRegistry, "com.test", "event", 1)
    result must beLeft(RegistryError.NotFound: RegistryError)
  }

  def e4 = {
    val result: Id[Either[RegistryError, Json]] =
      idLookupInstance.lookup(inMemoryRegistry, testKey)
    result must beRight
  }

  def e5 = {
    val embeddedKey = SchemaKey(
      "com.snowplowanalytics.iglu-test",
      "stock-item",
      "jsonschema",
      SchemaVer.Full(1, 0, 0)
    )
    val embedded = Registry.Embedded(
      Registry.Config("Test Embedded", 0, List("com.snowplowanalytics")),
      "/iglu-test-embedded"
    )
    val result: Id[Either[RegistryError, Json]] =
      idLookupInstance.lookup(embedded, embeddedKey)
    result must beRight
  }
}
