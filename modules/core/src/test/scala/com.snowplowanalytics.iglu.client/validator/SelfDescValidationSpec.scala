/*
 * Copyright (c) 2014-2021 Snowplow Analytics Ltd. All rights reserved.
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
import io.circe.Json
import io.circe.literal._

// Iglu Core
import com.snowplowanalytics.iglu.core.{SchemaKey, SchemaVer, SelfDescribingData}

// Specs2
import org.specs2.Specification
import org.specs2.matcher.ValidatedMatchers

import com.snowplowanalytics.iglu.client.SpecHelpers
import com.snowplowanalytics.iglu.client.SpecHelpers._

class SelfDescValidationSpec extends Specification with ValidatedMatchers {
  def is = s2"""

  This is a specification to test validation of self-describing JSONs

  validating a correct self-desc JSON should return the Unit in a Right $e1
  validating an incorrect self-desc JSON should return the validation errors in a Failure  $e3
  """

  val validJson =
    SelfDescribingData[Json](
      SchemaKey(
        "com.snowplowanalytics.iglu-test",
        "stock-item",
        "jsonschema",
        SchemaVer.Full(1, 0, 0)
      ),
      json"""{"id": "123-12", "name": "t-shirt", "price": 29.99 }"""
    )

  val invalidJson =
    SelfDescribingData[Json](
      SchemaKey(
        "com.snowplowanalytics.iglu-test",
        "stock-item",
        "jsonschema",
        SchemaVer.Full(1, 0, 0)
      ),
      json"""{ "id": "123-12", "newName": "t-shirt", "price": "tbc"}"""
    )

  def e1 = {
    val action = for {
      client <- SpecHelpers.TestClient
      result <- client.check(validJson).value
    } yield result must beRight(())

    action.unsafeRunSync()
  }

  def e3 = {
    val action = for {
      client <- SpecHelpers.TestClient
      result <- client.check(invalidJson).value
    } yield result must beLeft

    action.unsafeRunSync()
  }
}
