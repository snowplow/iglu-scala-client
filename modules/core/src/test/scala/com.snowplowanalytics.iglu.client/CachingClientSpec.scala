/*
 * Copyright (c) 2012-2022 Snowplow Analytics Ltd. All rights reserved.
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

import cats.effect.testing.specs2.CatsEffect
// circe
import io.circe.literal._

import com.snowplowanalytics.iglu.core.{SchemaKey, SchemaVer, SelfDescribingData}

// Specs2
import org.specs2.mutable.Specification

/** Similar to 'SchemaValidationSpec' and 'SelfDescValidationSpec' in validator package but using caching 'IgluCirceClient' client */
class CachingClientSpec extends Specification with CatsEffect {
  override def is = s2"""

  This is a specification to test client check

  validating a correct self-desc JSON should return the JSON in a Success $e1
  validating a correct self-desc JSON with JSON Schema with incorrect $$schema property should return Failure $e2
  validating an incorrect self-desc JSON should return the validation errors in a Failure  $e3

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

  val invalidJson =
    SelfDescribingData(
      SchemaKey(
        "com.snowplowanalytics.iglu-test",
        "stock-item",
        "jsonschema",
        SchemaVer.Full(1, 0, 0)
      ),
      json"""{ "id": "123-12", "newName": "t-shirt", "price": "tbc"}"""
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

  def e1 = {
    for {
      client <- SpecHelpers.CachingTestClient
      result <- client.check(validJson).value
    } yield result must beRight
  }

  def e2 = {
    for {
      client <- SpecHelpers.CachingTestClient
      result <- client.check(validJsonWithInvalidSchema).value
    } yield result must beLeft
  }

  def e3 = {
    for {
      client <- SpecHelpers.CachingTestClient
      result <- client.check(invalidJson).value
    } yield result must beLeft
  }

}
