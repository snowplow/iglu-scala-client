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
  // TODO: add a JSON which fails schema identification
                                                                                                                             end


  implicit val resolver = SpecHelpers.TestResolver

  val validJson = SpecHelpers.asJsonNode(
    """{"schema": "iglu:com.snowplowanalytics.iglu-test/stock-item/jsonschema/1-0-0", "data": { "id": "123-12", "name": "t-shirt", "price": 29.99 } }"""
    )

  val expectedKey = SchemaKey("com.snowplowanalytics.iglu-test", "stock-item", "jsonschema", "1-0-0")

  def e1 = validJson.validate(false) must beSuccessful(validJson)

  def e2 = validJson.validate(true) must beSuccessful(validJson.get("data"))

  def e3 = validJson.validateAndIdentifySchema(false) must beSuccessful((expectedKey, validJson))

  def e4 = validJson.validateAndIdentifySchema(true) must beSuccessful((expectedKey, validJson.get("data")))
}
