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
package com.snowplowanalytics.iglu.client
package validator

// Cats
import cats.data.NonEmptyList

// circe
import io.circe.Json
import io.circe.literal._
import io.circe.parser.parse

// Specs2
import org.specs2.Specification

class NumberRangeValidationSpec extends Specification {
  def is = s2"""

  This is a specification to test rejection of numbers outside the range of a 64-bit float

  invalidate a number larger than Double.MaxValue $e1
  invalidate a number smaller than -Double.MaxValue $e2
  invalidate a subnormal number $e3
  invalidate a negative subnormal number $e4
  invalidate a number that underflows to zero $e5
  validate the smallest normal number $e6
  validate the largest finite number $e7
  validate zero and negative zero $e8
  validate integers bigger than java.lang.Long.MAX_VALUE $e9
  validate ordinary numbers $e10
  do not apply the range check to numbers in the schema $e11
  find an out-of-range number nested inside the instance $e12
  """

  private val numberSchema: Json = json"""{ "type": "number" }"""

  private def instance(literal: String): Json =
    parse(literal).fold(e => throw new RuntimeException(s"Cannot parse $literal, $e"), identity)

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

  def e1 =
    CirceValidator.validate(instance("1e400"), numberSchema) must beLeft(outOfRange("1e400"))

  def e2 =
    CirceValidator.validate(instance("-1e400"), numberSchema) must beLeft(outOfRange("-1e400"))

  def e3 =
    CirceValidator.validate(instance("5e-324"), numberSchema) must beLeft(outOfRange("5e-324"))

  def e4 =
    CirceValidator.validate(instance("-5e-324"), numberSchema) must beLeft(outOfRange("-5e-324"))

  def e5 =
    CirceValidator.validate(instance("1e-400"), numberSchema) must beLeft(outOfRange("1e-400"))

  def e6 =
    CirceValidator.validate(
      instance("2.2250738585072014e-308"),
      numberSchema
    ) must beRight

  def e7 =
    CirceValidator.validate(
      instance("1.7976931348623157e308"),
      numberSchema
    ) must beRight

  def e8 = {
    val zero         = CirceValidator.validate(instance("0"), numberSchema) must beRight
    val zeroPoint    = CirceValidator.validate(instance("0.0"), numberSchema) must beRight
    val negativeZero = CirceValidator.validate(instance("-0.0"), numberSchema) must beRight

    zero and zeroPoint and negativeZero
  }

  def e9 =
    CirceValidator.validate(
      instance("9223372036854775809"),
      json"""{ "type": "integer" }"""
    ) must beRight

  def e10 = {
    val schema = json"""{ "type": "object" }"""
    val input  = instance("""{"a": 1, "b": -1.5, "c": 1e-307, "d": 3.4e38, "e": [0.1, 2]}""")

    CirceValidator.validate(input, schema) must beRight
  }

  def e11 = {
    val schema = json"""{ "type": "number", "maximum": 1e400 }"""

    CirceValidator.validate(instance("1"), schema) must beRight
  }

  def e12 = {
    val schema = json"""{ "type": "object" }"""
    val input  = instance("""{"a": {"b": [1, 5e-324]}}""")

    CirceValidator.validate(input, schema) must beLeft(outOfRange("5e-324"))
  }
}
