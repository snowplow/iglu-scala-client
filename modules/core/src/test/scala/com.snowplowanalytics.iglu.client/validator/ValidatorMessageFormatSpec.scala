/*
 * Copyright (c) 2014-2023 Snowplow Analytics Ltd. All rights reserved.
 *
 * This program is licensed to you under the Apache License Version 2.0,
 * and you may not use this file except in compliance with the Apache License Version 2.0.
 * You may obtain a copy of the Apache License Version 2.0 at http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the Apache License Version 2.0 is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the Apache License Version 2.0 for the specific language governing permissions and limitations there under.
 */
package com.snowplowanalytics.iglu.client.validator

import io.circe.literal._
import org.specs2.Specification

/**
 * Comprehensive test spec to ensure all validator types maintain consistent
 * error message formatting across json-schema-validator library versions.
 *
 * This spec tests validator types defined in CustomMessageFormatter to ensure:
 * 1. Message templates match expected format from version 1.0.76
 * 2. Argument positions ({0}, {1}, {2}) are correct
 * 3. If the library changes behavior, these tests will fail
 */
class ValidatorMessageFormatSpec extends Specification {
  def is = s2"""

  This specification tests error message formatting for validator types

  Numeric validators:
    minimum validator produces correct message format $e1
    maximum validator produces correct message format $e2
    multipleOf validator produces correct message format $e3

  String validators:
    minLength validator produces correct message format $e4
    maxLength validator produces correct message format $e5
    pattern validator produces correct message format $e6

  Array validators:
    minItems validator produces correct message format $e7
    maxItems validator produces correct message format $e8
    uniqueItems validator produces correct message format $e9
    items validator (via type error) produces correct message format $e10

  Object validators:
    minProperties validator produces correct message format $e11
    maxProperties validator produces correct message format $e12
    required validator produces correct message format $e13
    additionalProperties validator produces correct message format $e14

  Type validators:
    type validator produces correct message format $e15
    enum validator produces correct message format $e16

  Composition validators:
    allOf validator (via sub-validator) produces correct message format $e17
    oneOf validator produces correct message format $e18
    not validator produces correct message format $e19

  Format validators:
    format validator produces correct message format $e20
  """

  // Helper to check message matches expected pattern
  def checkMessage(message: String, pattern: String): Boolean = {
    // Pattern uses placeholders: PATH, ARG1, ARG2, etc.
    val regex = pattern
      .replace("PATH", "\\$.*?")
      .replace("ARG1", ".+")
      .replace("ARG2", ".+")
      .replace("ARG3", ".+")
    message.matches(regex)
  }

  // Numeric validators

  def e1 = {
    val schema = json"""{ "minimum": 5 }"""
    val input  = json"""3"""
    CirceValidator.validate(input, schema) match {
      case Left(ValidatorError.InvalidData(errors)) =>
        val report = errors.head
        report.keyword must beSome("minimum")
        report.message must beEqualTo("$: must have a minimum value of 5")
        report.targets must beEqualTo(List("5"))
      case other => ko(s"Expected InvalidData with minimum error, got: $other")
    }
  }

  def e2 = {
    val schema = json"""{ "maximum": 10 }"""
    val input  = json"""15"""
    CirceValidator.validate(input, schema) match {
      case Left(ValidatorError.InvalidData(errors)) =>
        val report = errors.head
        report.keyword must beSome("maximum")
        report.message must beEqualTo("$: must have a maximum value of 10")
        report.targets must beEqualTo(List("10"))
      case other => ko(s"Expected InvalidData with maximum error, got: $other")
    }
  }

  def e3 = {
    val schema = json"""{ "multipleOf": 5 }"""
    val input  = json"""7"""
    CirceValidator.validate(input, schema) match {
      case Left(ValidatorError.InvalidData(errors)) =>
        val report = errors.head
        report.keyword must beSome("multipleOf")
        report.message must startWith("$: must be multiple of ")
        report.targets.headOption must beSome
      case other => ko(s"Expected InvalidData with multipleOf error, got: $other")
    }
  }

  // String validators

  def e4 = {
    val schema = json"""{ "minLength": 5 }"""
    val input  = json""""abc""""
    CirceValidator.validate(input, schema) match {
      case Left(ValidatorError.InvalidData(errors)) =>
        val report = errors.head
        report.keyword must beSome("minLength")
        report.message must beEqualTo("$: must be at least 5 characters long")
        report.targets must beEqualTo(List("5"))
      case other => ko(s"Expected InvalidData with minLength error, got: $other")
    }
  }

  def e5 = {
    val schema = json"""{ "maxLength": 3 }"""
    val input  = json""""abcd""""
    CirceValidator.validate(input, schema) match {
      case Left(ValidatorError.InvalidData(errors)) =>
        val report = errors.head
        report.keyword must beSome("maxLength")
        report.message must beEqualTo("$: may only be 3 characters long")
        report.targets must beEqualTo(List("3"))
      case other => ko(s"Expected InvalidData with maxLength error, got: $other")
    }
  }

  def e6 = {
    val schema = json"""{ "pattern": "^[a-z]+$$" }"""
    val input  = json""""ABC123""""
    CirceValidator.validate(input, schema) match {
      case Left(ValidatorError.InvalidData(errors)) =>
        val report = errors.head
        report.keyword must beSome("pattern")
        report.message must beEqualTo("$: does not match the regex pattern ^[a-z]+$")
        report.targets must beEqualTo(List("^[a-z]+$"))
      case other => ko(s"Expected InvalidData with pattern error, got: $other")
    }
  }

  // Array validators

  def e7 = {
    val schema = json"""{ "minItems": 3 }"""
    val input  = json"""[1, 2]"""
    CirceValidator.validate(input, schema) match {
      case Left(ValidatorError.InvalidData(errors)) =>
        val report = errors.head
        report.keyword must beSome("minItems")
        report.message must beEqualTo("$: there must be a minimum of 3 items in the array")
        // In 1.5.8, minItems returns [minItems, actualItems]
        report.targets.headOption must beSome("3")
      case other => ko(s"Expected InvalidData with minItems error, got: $other")
    }
  }

  def e8 = {
    val schema = json"""{ "maxItems": 2 }"""
    val input  = json"""[1, 2, 3]"""
    CirceValidator.validate(input, schema) match {
      case Left(ValidatorError.InvalidData(errors)) =>
        val report = errors.head
        report.keyword must beSome("maxItems")
        report.message must beEqualTo("$: there must be a maximum of 2 items in the array")
        // In 1.5.8, maxItems returns [maxItems, actualItems]
        report.targets.headOption must beSome("2")
      case other => ko(s"Expected InvalidData with maxItems error, got: $other")
    }
  }

  def e9 = {
    val schema = json"""{ "uniqueItems": true }"""
    val input  = json"""[1, 2, 2, 3]"""
    CirceValidator.validate(input, schema) match {
      case Left(ValidatorError.InvalidData(errors)) =>
        val report = errors.head
        report.keyword must beSome("uniqueItems")
        report.message must beEqualTo("$: the items in the array must be unique")
      case other => ko(s"Expected InvalidData with uniqueItems error, got: $other")
    }
  }

  def e10 = {
    val schema = json"""{
      "items": { "type": "string" }
    }"""
    val input = json"""["a", "b", 3]"""
    CirceValidator.validate(input, schema) match {
      case Left(ValidatorError.InvalidData(errors)) =>
        val report = errors.head
        report.keyword must beSome("type")
        report.message must beEqualTo("$[2]: integer found, string expected")
        report.targets must beEqualTo(List("integer", "string"))
      case other => ko(s"Expected InvalidData with type error, got: $other")
    }
  }

  // Object validators

  def e11 = {
    val schema = json"""{ "minProperties": 3 }"""
    val input  = json"""{ "a": 1, "b": 2 }"""
    CirceValidator.validate(input, schema) match {
      case Left(ValidatorError.InvalidData(errors)) =>
        val report = errors.head
        report.keyword must beSome("minProperties")
        report.message must beEqualTo("$: should have a minimum of 3 properties")
        report.targets must beEqualTo(List("3"))
      case other => ko(s"Expected InvalidData with minProperties error, got: $other")
    }
  }

  def e12 = {
    val schema = json"""{ "maxProperties": 2 }"""
    val input  = json"""{ "a": 1, "b": 2, "c": 3 }"""
    CirceValidator.validate(input, schema) match {
      case Left(ValidatorError.InvalidData(errors)) =>
        val report = errors.head
        report.keyword must beSome("maxProperties")
        report.message must beEqualTo("$: may only have a maximum of 2 properties")
        report.targets must beEqualTo(List("2"))
      case other => ko(s"Expected InvalidData with maxProperties error, got: $other")
    }
  }

  def e13 = {
    val schema = json"""{
      "properties": {
        "name": { "type": "string" }
      },
      "required": ["name"]
    }"""
    val input = json"""{ "age": 30 }"""
    CirceValidator.validate(input, schema) match {
      case Left(ValidatorError.InvalidData(errors)) =>
        val report = errors.head
        report.keyword must beSome("required")
        report.message must beEqualTo("$.name: is missing but it is required")
        report.targets must beEqualTo(List("name"))
      case other => ko(s"Expected InvalidData with required error, got: $other")
    }
  }

  def e14 = {
    val schema = json"""{
      "properties": {
        "name": { "type": "string" }
      },
      "additionalProperties": false
    }"""
    val input = json"""{ "name": "John", "age": 30 }"""
    CirceValidator.validate(input, schema) match {
      case Left(ValidatorError.InvalidData(errors)) =>
        val report = errors.head
        report.keyword must beSome("additionalProperties")
        report.message must beEqualTo(
          "$.age: is not defined in the schema and the schema does not allow additional properties"
        )
        report.targets must beEqualTo(List("age"))
      case other => ko(s"Expected InvalidData with additionalProperties error, got: $other")
    }
  }

  // Type validators

  def e15 = {
    val schema = json"""{ "type": "string" }"""
    val input  = json"""123"""
    CirceValidator.validate(input, schema) match {
      case Left(ValidatorError.InvalidData(errors)) =>
        val report = errors.head
        report.keyword must beSome("type")
        report.message must beEqualTo("$: integer found, string expected")
        report.targets must beEqualTo(List("integer", "string"))
      case other => ko(s"Expected InvalidData with type error, got: $other")
    }
  }

  def e16 = {
    val schema = json"""{ "enum": ["red", "green", "blue"] }"""
    val input  = json""""yellow""""
    CirceValidator.validate(input, schema) match {
      case Left(ValidatorError.InvalidData(errors)) =>
        val report = errors.head
        report.keyword must beSome("enum")
        report.message must contain("does not have a value in the enumeration")
      case other => ko(s"Expected InvalidData with enum error, got: $other")
    }
  }

  // Composition validators

  def e17 = {
    val schema = json"""{
      "allOf": [
        { "type": "string" },
        { "minLength": 5 }
      ]
    }"""
    val input = json""""abc""""
    CirceValidator.validate(input, schema) match {
      case Left(ValidatorError.InvalidData(errors)) =>
        val report = errors.head
        report.keyword must beSome("minLength")
        report.message must beEqualTo("$: must be at least 5 characters long")
      case other => ko(s"Expected InvalidData with minLength error from allOf, got: $other")
    }
  }

  def e18 = {
    val schema = json"""{
      "oneOf": [
        { "type": "number", "multipleOf": 5 },
        { "type": "number", "multipleOf": 3 }
      ]
    }"""
    val input = json"""15"""
    CirceValidator.validate(input, schema) match {
      case Left(ValidatorError.InvalidData(errors)) =>
        val report = errors.head
        report.keyword must beSome("oneOf")
        report.message must contain("should be valid to one and only one of schema")
      case other => ko(s"Expected InvalidData with oneOf error, got: $other")
    }
  }

  def e19 = {
    val schema = json"""{
      "not": { "type": "string" }
    }"""
    val input = json""""test""""
    CirceValidator.validate(input, schema) match {
      case Left(ValidatorError.InvalidData(errors)) =>
        val report = errors.head
        report.keyword must beSome("not")
        report.message must contain("should not be valid to the schema")
      case other => ko(s"Expected InvalidData with not error, got: $other")
    }
  }

  // Format validators

  def e20 = {
    val schema = json"""{ "format": "ipv4" }"""
    val input  = json""""not-an-ip""""
    CirceValidator.validate(input, schema) match {
      case Left(ValidatorError.InvalidData(errors)) =>
        val report = errors.head
        report.keyword must beSome("format")
        report.message must contain("does not match the ipv4 pattern")
        report.targets.headOption must beSome("ipv4")
      case other => ko(s"Expected InvalidData with format error, got: $other")
    }
  }
}
