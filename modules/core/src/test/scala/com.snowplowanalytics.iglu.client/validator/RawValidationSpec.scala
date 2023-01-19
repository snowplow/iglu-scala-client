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
import org.specs2.matcher.DataTables

class RawValidationSpec extends Specification with DataTables {
  def is = s2"""

  This is a specification to test the basic Validatable functionality

  beer-schema.json validates valid instances $e1
  beer-schema.json invalidates invalid instances $e2
  maxLength invalidates longer string $e3
  maxLength does not try to validate integer $e4
  additionalProperties invalidates unexpected property $e5
  ipv4 format invalidates plain string $e6
  validate integers bigger than java.lang.Long.MAX_VALUE $e7
  validate null in [array, null] type $e8
  invalidate stringly integer with integer type $e9
  validate integer with number type $e10
  """

  val simpleSchemaResult: Json =
    parse(
      scala.io.Source
        .fromInputStream(getClass.getResourceAsStream("/raw-jsonschema/beer-schema.json"))
        .mkString
    )
      .fold(e => throw new RuntimeException(s"Cannot parse beer-schema.json, $e"), identity)

  def e1 =
    foreach(
      List(
        json"""{"country": "JP", "beers": ["Asahi", "Orion", "..."]}""",
        json"""{"country": "AQ", "beers": []}""",
        json"""{"country":"GR","beers":["Fix","Mythos"]}""",
        json"""{"country": "fr","beers": ["Jenlain"]}"""
      )
    ) { json =>
      val result = CirceValidator.validate(json, simpleSchemaResult)

      result must beRight
    }

  def e2 = {
    val nonStringInput         = json"""{"country": 123, "beers": []}"""
    val missingKeyInput        = json"""{"country": "cy"}"""
    val heterogeneusArrayInput = json"""{"country": "GB", "beers": ["ale", false]}"""
    val doubleErrorInput       = json"""{"country": 23, "beers": ["ale", false]}"""

    val nonStringExpected = ValidatorError.InvalidData(
      NonEmptyList.of(
        ValidatorReport(
          "$.country: integer found, string expected",
          Some("$.country"),
          List("integer", "string"),
          Some("type")
        )
      )
    )
    val missingKeyExpected = ValidatorError.InvalidData(
      NonEmptyList.of(
        ValidatorReport(
          "$.beers: is missing but it is required",
          Some("$"),
          List("beers"),
          Some("required")
        )
      )
    )
    val heterogeneusArrayExpected = ValidatorError.InvalidData(
      NonEmptyList.of(
        ValidatorReport(
          "$.beers[1]: boolean found, string expected",
          Some("$.beers[1]"),
          List("boolean", "string"),
          Some("type")
        )
      )
    )
    val doubleErrorExpected = ValidatorError.InvalidData(
      NonEmptyList.of(
        ValidatorReport(
          "$.country: integer found, string expected",
          Some("$.country"),
          List("integer", "string"),
          Some("type")
        ),
        ValidatorReport(
          "$.beers[1]: boolean found, string expected",
          Some("$.beers[1]"),
          List("boolean", "string"),
          Some("type")
        )
      )
    )

    val nonString =
      CirceValidator.validate(nonStringInput, simpleSchemaResult) must beLeft(nonStringExpected)
    val missingKey =
      CirceValidator.validate(missingKeyInput, simpleSchemaResult) must beLeft(missingKeyExpected)
    val heterogeneusArray =
      CirceValidator.validate(heterogeneusArrayInput, simpleSchemaResult) must beLeft(
        heterogeneusArrayExpected
      )
    val doubleError =
      CirceValidator.validate(doubleErrorInput, simpleSchemaResult) must beLeft(doubleErrorExpected)

    nonString and missingKey and heterogeneusArray and doubleError
  }

  def e3 = {
    val schema = json"""
      {
        "properties": {
          "shortKey": { "maxLength": 3 }
        }
      }
      """
    val input = json"""{"shortKey": "aaaa" }"""
    val expected = ValidatorError.InvalidData(
      NonEmptyList.of(
        ValidatorReport(
          "$.shortKey: may only be 3 characters long",
          Some("$.shortKey"),
          List("3"),
          Some("maxLength")
        )
      )
    )

    CirceValidator.validate(input, schema) must beLeft(expected)
  }

  def e4 = {
    val schema = json"""
      {
        "properties": {
          "shortKey": { "maxLength": 3 }
        }
      }
      """
    val input = json"""{"shortKey": 5 }"""

    CirceValidator.validate(input, schema) must beRight
  }

  def e5 = {
    val schema = json"""
      {
        "properties": {
          "twoKeys": {
            "properties": {
              "one": {},
              "two": {}
            },
            "additionalProperties": false
          }
        }
      }
      """
    val input = json"""{"twoKeys": {"one": 1, "two": 2, "three": 3} }"""
    val expected = ValidatorError.InvalidData(
      NonEmptyList.of(
        ValidatorReport(
          "$.twoKeys.three: is not defined in the schema and the schema does not allow additional properties",
          Some("$.twoKeys"),
          List("three"),
          Some("additionalProperties")
        )
      )
    )

    CirceValidator.validate(input, schema) must beLeft(expected)
  }

  def e6 = {
    val schema = json"""
      {
        "properties": {
          "address": { "format": "ipv4" }
        }
      }
      """
    val input = json"""{"address": "non-ip" }"""
    val expected = ValidatorError.InvalidData(
      NonEmptyList.of(
        ValidatorReport(
          "$.address: does not match the ipv4 pattern ^(([0-9]|[1-9][0-9]|1[0-9][0-9]|2[0-4][0-9]|25[0-5])\\.){3}([0-9]|[1-9][0-9]|1[0-9][0-9]|2[0-4][0-9]|25[0-5])$",
          Some("$.address"),
          List(
            "ipv4",
            "^(([0-9]|[1-9][0-9]|1[0-9][0-9]|2[0-4][0-9]|25[0-5])\\.){3}([0-9]|[1-9][0-9]|1[0-9][0-9]|2[0-4][0-9]|25[0-5])$"
          ),
          Some("format")
        )
      )
    )

    CirceValidator.validate(input, schema) must beLeft(expected)
  }

  def e7 = {
    val schema = json"""{ "type": "integer" }"""
    val input  = json"""9223372036854775809"""
    CirceValidator.validate(input, schema) must beRight
  }

  def e8 = {
    val schema = json"""{ "type": ["array", "null"], "items": {"type": "object"} }"""
    val input  = json"""null"""
    CirceValidator.validate(input, schema) must beRight
  }

  def e9 = {
    val schema = json"""{ "type": "integer" }"""
    val input  = json""""5""""
    CirceValidator.validate(input, schema) must beLeft
  }

  def e10 = {
    val schema = json"""{ "type": "number" }"""
    val input  = json"""5"""
    CirceValidator.validate(input, schema) must beRight
  }
}
