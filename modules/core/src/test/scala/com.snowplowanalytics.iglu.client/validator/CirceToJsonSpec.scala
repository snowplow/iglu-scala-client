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
package com.snowplowanalytics.iglu.client.validator

import io.circe.Json
import io.circe.jackson.snowplow.{CirceToJsonError, circeToJackson}
import io.circe.literal._

import org.specs2.Specification

class CirceToJsonSpec extends Specification {
  def is = s2"""
  circeToJackson should
    convert null                               $e1
    convert boolean                            $e2
    convert string                             $e3
    convert integer number                     $e4
    convert double number                      $e5
    convert array                              $e6
    convert object                             $e7
    fail when max depth exceeded               $e8
    handle deeply nested JSON within limits     $e9
  CirceToJsonError should
    produce correct message for MaxDepthExceeded  $e10
    toInvalidData should create InvalidData       $e11
    toInvalidSchema should create InvalidSchema   $e12
    toSchemaIssue should create SchemaIssue       $e13
  CirceValidator.compileJsonSchema should
    succeed for a valid schema                    $e14
    fail for an invalid schema                    $e15
  """

  def e1 = {
    val result = circeToJackson(Json.Null, 10)
    result must beRight
  }

  def e2 = {
    val result = circeToJackson(Json.True, 10)
    result must beRight
  }

  def e3 = {
    val result = circeToJackson(Json.fromString("hello"), 10)
    result must beRight
  }

  def e4 = {
    val result = circeToJackson(Json.fromInt(42), 10)
    result must beRight
  }

  def e5 = {
    val result = circeToJackson(Json.fromDoubleOrNull(3.14), 10)
    result must beRight
  }

  def e6 = {
    val result = circeToJackson(Json.arr(Json.fromInt(1), Json.fromInt(2)), 10)
    result must beRight
  }

  def e7 = {
    val result = circeToJackson(json"""{"key": "value"}""", 10)
    result must beRight
  }

  def e8 = {
    val result = circeToJackson(Json.obj("a" -> Json.obj("b" -> Json.True)), 0)
    result must beLeft(CirceToJsonError.MaxDepthExceeded: CirceToJsonError)
  }

  def e9 = {
    val nested = json"""{"a": {"b": {"c": true}}}"""
    val result = circeToJackson(nested, 5)
    result must beRight
  }

  def e10 =
    CirceToJsonError.MaxDepthExceeded.message must_== "Maximum allowed JSON depth exceeded"

  def e11 = {
    val invalidData = CirceToJsonError.MaxDepthExceeded.toInvalidData
    invalidData must beAnInstanceOf[ValidatorError.InvalidData]
  }

  def e12 = {
    val invalidSchema = CirceToJsonError.MaxDepthExceeded.toInvalidSchema
    invalidSchema must beAnInstanceOf[ValidatorError.InvalidSchema]
  }

  def e13 = {
    val issue = CirceToJsonError.MaxDepthExceeded.toSchemaIssue
    (issue.path must_== "/") and
      (issue.message must_== "Maximum allowed JSON depth exceeded")
  }

  def e14 = {
    val schema = json"""{
      "type": "object",
      "properties": {
        "name": { "type": "string" }
      }
    }"""
    val result = CirceValidator.compileJsonSchema(schema, 100)
    result must beRight
  }

  def e15 = {
    val schema = json"""{
      "type": "object",
      "properties": "not-an-object"
    }"""
    val result = CirceValidator.compileJsonSchema(schema, 100)
    result must beLeft
  }
}
