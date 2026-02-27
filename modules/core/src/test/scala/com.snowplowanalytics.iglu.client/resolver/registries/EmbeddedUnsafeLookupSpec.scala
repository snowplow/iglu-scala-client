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

import com.snowplowanalytics.iglu.core.{SchemaKey, SchemaVer}
import com.snowplowanalytics.iglu.client.resolver.registries.JavaNetRegistryLookup._

import org.specs2.Specification

class EmbeddedUnsafeLookupSpec extends Specification {
  def is = s2"""
  Embedded unsafeLookup via Id lookup should
    find existing schema                        $e1
    return NotFound for missing schema          $e2
    return NotFound for missing resource        $e3
    return error for corrupted schema           $e4
  Embedded unsafeList via Id list should
    list schemas for existing folder            $e5
    return NotFound for missing folder          $e6
  """

  val testEmbedded: Registry.Embedded = Registry.Embedded(
    Registry.Config("Test Embedded", 0, List("com.snowplowanalytics")),
    "/iglu-test-embedded"
  )

  def e1 = {
    val key = SchemaKey(
      "com.snowplowanalytics.iglu-test",
      "stock-item",
      "jsonschema",
      SchemaVer.Full(1, 0, 0)
    )
    val result: Id[Either[RegistryError, _]] = idLookupInstance.lookup(testEmbedded, key)
    result must beRight
  }

  def e2 = {
    val key = SchemaKey("com.nonexistent", "missing", "jsonschema", SchemaVer.Full(1, 0, 0))
    val result: Id[Either[RegistryError, _]] = idLookupInstance.lookup(testEmbedded, key)
    result must beLeft
  }

  def e3 = {
    val badEmbedded = Registry.Embedded(
      Registry.Config("Bad Embedded", 0, Nil),
      "/nonexistent-path"
    )
    val key = SchemaKey("com.test", "schema", "jsonschema", SchemaVer.Full(1, 0, 0))
    val result: Id[Either[RegistryError, _]] = idLookupInstance.lookup(badEmbedded, key)
    result must beLeft
  }

  def e4 = {
    val key = SchemaKey(
      "com.snowplowanalytics.iglu-test",
      "corrupted_schema",
      "jsonschema",
      SchemaVer.Full(1, 0, 0)
    )
    val result: Id[Either[RegistryError, _]] = idLookupInstance.lookup(testEmbedded, key)
    result must beLeft
  }

  def e5 = {
    val result: Id[Either[RegistryError, _]] =
      idLookupInstance.list(
        testEmbedded,
        "com.snowplowanalytics.iglu-test",
        "test-embedded-list",
        1
      )
    result must beRight
  }

  def e6 = {
    val result: Id[Either[RegistryError, _]] =
      idLookupInstance.list(testEmbedded, "com.nonexistent", "missing", 1)
    result must beLeft
  }
}
