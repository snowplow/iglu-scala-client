/*
 * Copyright (c) 2014 Snowplow Analytics Ltd. All rights reserved.
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
package validation

// This project
import ValidatableJsonMethods._
import validation.ProcessingMessageMethods._

// Specs2
import org.specs2.Specification
import org.specs2.scalaz.ValidationMatchers

class SelfDescValidationSpec extends Specification with ValidationMatchers { def is =

  "This is a specification to test validation of self-describing JsonNodes"                                                  ^
                                                                                                                            p^
  "validating a correct self-desc JSON should return the JSON in a Success"                                                  ! e1^
  "validating a correct self-desc JSON should return only the JSON's data field in a Success if requested"                   ! e2^
  "validating & identifying a correct self-desc JSON should return the JSON & the identified SchemaKey"                      ! e3^
  "validating & identifying a correct self-desc JSON should return only the JSON's data field & identified key if requested" ! e4^
  // TODO: add an invalid JSON
  "verifying & validating a correct self-desc JSON vs the correct schema should return the JSON in a Success"                ! e5^
  "verifying & validating a correct self-desc JSON vs the correct schema should return the JSON's data field in a Success"   ! e6^
  "verifying a self-desc JSON vs the incorrect schema should return a Failure message"                                       ! e7^
                                                                                                                             end

  implicit val resolver = SpecHelpers.TestResolver

  val validJson = SpecHelpers.asJsonNode(
    """{"schema": "iglu:com.snowplowanalytics.iglu-test/stock-item/jsonschema/1-0-0", "data": { "id": "123-12", "name": "t-shirt", "price": 29.99 } }"""
    )

  def e1 = validJson.validate(false) must beSuccessful(validJson)

  def e2 = validJson.validate(true) must beSuccessful(validJson.get("data"))

  val expectedKey = SchemaKey("com.snowplowanalytics.iglu-test", "stock-item", "jsonschema", "1-0-0")

  def e3 = validJson.validateAndIdentifySchema(false) must beSuccessful((expectedKey, validJson))

  def e4 = validJson.validateAndIdentifySchema(true) must beSuccessful((expectedKey, validJson.get("data")))

  def e5 = validJson.verifySchemaAndValidate(expectedKey, false) must beSuccessful(validJson)

  def e6 = validJson.verifySchemaAndValidate(expectedKey, true) must beSuccessful(validJson.get("data"))

  def e7 = {
    val incorrectKey = SchemaKey("com.snowplowanalytics.iglu-test", "stock-item", "jsonschema", "2-0-0")
    val expected = "Verifying schema as iglu:com.snowplowanalytics.iglu-test/stock-item/jsonschema/2-0-0 failed: found iglu:com.snowplowanalytics.iglu-test/stock-item/jsonschema/1-0-0".toProcessingMessageNel

    validJson.verifySchemaAndValidate(incorrectKey, false).leftMap(_.map(_.toString)) must beFailing(expected.map(_.toString))
  }
}
