/*
 * Copyright (c) 2012-2023 Snowplow Analytics Ltd. All rights reserved.
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
package com.snowplowanalytics.iglu.client.validator

// circe
import cats.effect.IO
import cats.effect.testing.specs2.CatsEffect
import com.snowplowanalytics.iglu.client.Client
import com.snowplowanalytics.iglu.client.resolver.Resolver
import io.circe.literal._

// Specs2
import com.snowplowanalytics.iglu.client.SpecHelpers
import com.snowplowanalytics.iglu.core.{SchemaKey, SchemaVer, SelfDescribingData}
import org.specs2.Specification

class SchemaValidationSpec extends Specification with CatsEffect {
  def is = s2"""

  This is a specification to test Schema Validation

  validating a correct self-desc JSON should return the JSON in a Success $e1
  validating a correct self-desc JSON with JSON Schema with incorrect $$schema property should return Failure $e2
  validating a unexpected self-desc JSON with valid JSON Schema should return Failure $e3
  """

  val validJson =
    SelfDescribingData(
      SchemaKey(
        "com.snowplowanalytics.iglu-test",
        "stock-item",
        "jsonschema",
        SchemaVer.Full(1, 0, 0)
      ),
      json"""{"id": "123-12", "name": "t-shirt", "price": 29.99 }"""
    )

  val validJsonWithInvalidSchema =
    SelfDescribingData(
      SchemaKey(
        "com.snowplowanalytics.iglu-test",
        "invalid-protocol",
        "jsonschema",
        SchemaVer.Full(1, 0, 0)
      ),
      json"""{"schema": "iglu://jsonschema/1-0-0", "data": { "id": 0 } }"""
    )

  val validJsonWithSchemaWithInvalidMetaSchema =
    SelfDescribingData(
      SchemaKey(
        "com.snowplowanalytics.self-desc",
        "schema",
        "jsonschema",
        SchemaVer.Full(1, 0, 0)
      ),
      json"""{"schema": "iglu:com.snowplowanalytics.self-desc/schema/jsonschema/1-0-0", "data": { "id": 0 } }"""
    )

  val testResolver = SpecHelpers.TestResolver

  def e1 = {
    for {
      client <- SpecHelpers.TestClient
      result <- client.check(validJson).value
    } yield result must beRight
  }

  def e2 = {
    for {
      client <- SpecHelpers.TestClient
      result <- client.check(validJsonWithInvalidSchema).value
    } yield result must beLeft
  }

  def e3 = {
    // todo: Improve this
    val TestResolver = Resolver.init[IO](10, None, SpecHelpers.IgluCentral)
    val TestClient   = for { resolver <- TestResolver } yield Client(resolver, CirceValidator)
    for {
      client <- TestClient
      result <- client.check(validJsonWithSchemaWithInvalidMetaSchema).value
    } yield result must beLeft
  }
}
