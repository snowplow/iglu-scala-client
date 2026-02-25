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

class ValidatorSpec extends Specification {
  def is = s2"""
  CirceValidator as a Validator should
    isValidSchema should return true for valid schema    $e1
    isValidSchema should return false for invalid schema $e2
    validateSchema should return Right for valid schema  $e3
    validateSchema should return Left for invalid schema $e4
    checkSchema with maxJsonDepth should work             $e5
  """

  def e1 = {
    val schema = json"""{
      "type": "object",
      "properties": {
        "name": { "type": "string" }
      }
    }"""
    CirceValidator.isValidSchema(schema) must beTrue
  }

  def e2 = {
    val schema = json"""{
      "type": "object",
      "properties": "not-an-object"
    }"""
    CirceValidator.isValidSchema(schema) must beFalse
  }

  def e3 = {
    val schema = json"""{
      "type": "object",
      "properties": {
        "id": { "type": "integer" }
      }
    }"""
    CirceValidator.validateSchema(schema) must beRight(())
  }

  def e4 = {
    val schema = json"""{
      "type": "object",
      "properties": "bad"
    }"""
    CirceValidator.validateSchema(schema) must beLeft
  }

  def e5 = {
    val schema = json"""{
      "type": "object",
      "properties": {
        "name": { "type": "string" }
      }
    }"""
    val issues = CirceValidator.checkSchema(schema, 100)
    issues must beEmpty
  }
}
