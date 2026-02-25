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

import java.net.URI

import io.circe.literal._

import com.snowplowanalytics.iglu.core.{SchemaKey, SchemaVer}

import org.specs2.Specification

class RegistrySpec extends Specification {
  def is = s2"""
  Registry should
    parse embedded configuration              $e1
    parse HTTP configuration                  $e2
    parse HTTP configuration with apikey      $e3
    have correct classPriority for Embedded   $e4
    have correct classPriority for Http       $e5
    have correct classPriority for InMemory   $e6
    Config vendorMatched should match prefix  $e7
    Config vendorMatched should not match     $e8
    IgluCentral should have priority 10       $e9
    EmbeddedRegistry should be named correctly $e10
    toPath should construct correct path       $e11
    toSubpath with model should work           $e12
    toSubpath without model should work        $e13
    inMemoryLookup should find matching schema $e14
    inMemoryLookup should return NotFound for missing $e15
  """

  def e1 = {
    val config = json"""{
      "name": "Test Embedded",
      "priority": 5,
      "vendorPrefixes": ["com.test"],
      "connection": {
        "embedded": {
          "path": "/test-path"
        }
      }
    }"""
    val expected = Registry.Embedded(
      config = Registry.Config("Test Embedded", 5, List("com.test")),
      path = "/test-path"
    )
    Registry.parse(config) must beRight(expected)
  }

  def e2 = {
    val config = json"""{
      "name": "Test HTTP",
      "priority": 10,
      "vendorPrefixes": ["com.example"],
      "connection": {
        "http": {
          "uri": "http://example.com"
        }
      }
    }"""
    val result = Registry.parse(config)
    result must beRight[Registry].which(_.isInstanceOf[Registry.Http])
  }

  def e3 = {
    val config = json"""{
      "name": "Test HTTP with key",
      "priority": 10,
      "vendorPrefixes": [],
      "connection": {
        "http": {
          "uri": "http://example.com",
          "apikey": "abc-123"
        }
      }
    }"""
    val result = Registry.parse(config)
    result must beRight[Registry].which { r =>
      r.isInstanceOf[Registry.Http] && {
        val http = r.asInstanceOf[Registry.Http]
        http.http.apikey.contains("abc-123")
      }
    }
  }

  def e4 = {
    val embedded = Registry.Embedded(Registry.Config("test", 0, Nil), "/path")
    embedded.classPriority must_== 2
  }

  def e5 = {
    val http = Registry.Http(
      Registry.Config("test", 0, Nil),
      Registry.HttpConnection(URI.create("http://localhost"), None)
    )
    http.classPriority must_== 100
  }

  def e6 = {
    val inMemory = Registry.InMemory(Registry.Config("test", 0, Nil), Nil)
    inMemory.classPriority must_== 1
  }

  def e7 = {
    val config = Registry.Config("test", 0, List("com.snowplowanalytics"))
    config.vendorMatched("com.snowplowanalytics.iglu") must beTrue
  }

  def e8 = {
    val config = Registry.Config("test", 0, List("com.snowplowanalytics"))
    config.vendorMatched("com.example.test") must beFalse
  }

  def e9 =
    Registry.IgluCentral.config.instancePriority must_== 10

  def e10 =
    Registry.EmbeddedRegistry.config.name must_== "Iglu Client Embedded"

  def e11 = {
    val key  = SchemaKey("com.test", "myschema", "jsonschema", SchemaVer.Full(1, 0, 0))
    val path = RegistryLookup.toPath("/base", key)
    path must_== "/base/schemas/com.test/myschema/jsonschema/1-0-0"
  }

  def e12 = {
    val path = RegistryLookup.toSubpath("/base/", "com.test", "myschema", 1)
    path must_== "/base/schemas/com.test/myschema/jsonschema/1"
  }

  def e13 = {
    val path = RegistryLookup.toSubpath("/base/", "com.test", "myschema")
    path must_== "/base/schemas/com.test/myschema/jsonschema"
  }

  def e14 = {
    import com.snowplowanalytics.iglu.core.{SchemaMap, SelfDescribingSchema}
    import io.circe.Json
    val key    = SchemaKey("com.test", "myschema", "jsonschema", SchemaVer.Full(1, 0, 0))
    val schema = SelfDescribingSchema(SchemaMap(key), Json.obj())
    val result = RegistryLookup.inMemoryLookup(List(schema), key)
    result must beRight
  }

  def e15 = {
    val key    = SchemaKey("com.test", "myschema", "jsonschema", SchemaVer.Full(1, 0, 0))
    val result = RegistryLookup.inMemoryLookup(Nil, key)
    result must beLeft(RegistryError.NotFound: RegistryError)
  }
}
