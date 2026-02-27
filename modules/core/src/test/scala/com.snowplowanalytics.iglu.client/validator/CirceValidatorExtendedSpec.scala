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

import io.circe.literal._

import org.specs2.Specification

class CirceValidatorExtendedSpec extends Specification {
  def is = s2"""
  CirceValidator.validate should
    succeed for valid data against schema               $validateSuccess
    fail for invalid data against schema                $validateFailure
    fail when schema is not a valid JSON Schema         $validateBadSchema
  CirceValidator.checkSchema should
    return empty list for valid schema                  $checkSchemaValid
    return issues for schema with invalid properties    $checkSchemaInvalid
    return issue for deeply nested schema exceeding depth $checkSchemaDepthExceeded
  CirceValidator.compileJsonSchema should
    compile a valid schema                              $compileValid
    fail for invalid schema                             $compileInvalid
  """

  val validSchema = json"""{
    "type": "object",
    "properties": {
      "name": {"type": "string"},
      "age": {"type": "integer"}
    },
    "required": ["name"],
    "additionalProperties": false
  }"""

  def validateSuccess = {
    val data = json"""{"name": "Alice", "age": 30}"""
    CirceValidator.validate(data, validSchema) must beRight(())
  }

  def validateFailure = {
    val data = json"""{"name": 42, "age": "not-an-int"}"""
    CirceValidator.validate(data, validSchema) must beLeft
  }

  def validateBadSchema = {
    // Schema that parses as JSON but isn't a valid JSON schema structure
    val badSchema = json"""{"type": 12345}"""
    val data      = json"""{"name": "Bob"}"""
    // This should fail during schema evaluation
    CirceValidator.validate(data, badSchema) must beLeft
  }

  def checkSchemaValid =
    CirceValidator.checkSchema(validSchema, 100) must beEmpty

  def checkSchemaInvalid = {
    val schema = json"""{
      "type": "object",
      "properties": "not-an-object"
    }"""
    CirceValidator.checkSchema(schema, 100) must not(beEmpty)
  }

  def checkSchemaDepthExceeded = {
    // Schema itself is too deeply nested for the given depth limit
    val schema = json"""{"type": "object"}"""
    CirceValidator.checkSchema(schema, 0) must not(beEmpty)
  }

  def compileValid = {
    val schema = json"""{
      "type": "object",
      "properties": {
        "id": {"type": "string"}
      }
    }"""
    CirceValidator.compileJsonSchema(schema, 100) must beRight
  }

  def compileInvalid = {
    val schema = json"""{
      "type": "object",
      "properties": "bad"
    }"""
    CirceValidator.compileJsonSchema(schema, 100) must beLeft
  }
}
