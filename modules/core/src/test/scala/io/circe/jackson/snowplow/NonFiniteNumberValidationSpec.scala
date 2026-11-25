/*
 * Copyright (c) 2014-2026 Snowplow Analytics Ltd. All rights reserved.
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
// `JsonDouble` is private to circe, so this spec must sit inside the package to build a
// non-finite number at all.
package io.circe

// Cats
import cats.data.NonEmptyList

// Iglu
import com.snowplowanalytics.iglu.client.validator.{CirceValidator, ValidatorError, ValidatorReport}

// circe
import io.circe.literal._

// Specs2
import org.specs2.Specification

class NonFiniteNumberValidationSpec extends Specification {
  def is = s2"""

  This is a specification to test rejection of numbers a JSON document cannot denote

  invalidate NaN $e1
  invalidate positive infinity $e2
  invalidate negative infinity $e3
  """

  private val numberSchema: Json = json"""{ "type": "number" }"""

  private def outOfRange(value: String) =
    ValidatorError.InvalidData(
      NonEmptyList.one(
        ValidatorReport(
          s"Number $value cannot be represented as a 64-bit floating point number",
          Some("/"),
          List.empty,
          None
        )
      )
    )

  private def instance(value: Double): Json =
    Json.fromJsonNumber(JsonDouble(value))

  def e1 =
    CirceValidator.validate(instance(Double.NaN), numberSchema) must beLeft(outOfRange("NaN"))

  def e2 =
    CirceValidator.validate(
      instance(Double.PositiveInfinity),
      numberSchema
    ) must beLeft(outOfRange("Infinity"))

  def e3 =
    CirceValidator.validate(
      instance(Double.NegativeInfinity),
      numberSchema
    ) must beLeft(outOfRange("-Infinity"))
}
