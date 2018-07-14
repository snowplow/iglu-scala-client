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

// Scalaz
import scalaz._
import Scalaz._

// This project
import ValidatableJsonMethods._
import validation.ProcessingMessageMethods._

// Specs2
import org.specs2.Specification
import org.specs2.scalaz.ValidationMatchers

class SelfDescValidationSpec extends Specification with ValidationMatchers {
  def is = s2"""

  This is a specification to test validation of self-describing JsonNodes

  validating a correct self-desc JSON should return the JSON in a Success  $e1
  validating a correct self-desc JSON should return only the JSON's data field in a Success if requested  $e2
  validating an incorrect self-desc JSON should return the validation errors in a Failure  $e3
  validating & identifying a correct self-desc JSON should return the JSON & the identified SchemaCriterion  $e4
  validating & identifying a correct self-desc JSON should return only the JSON's data field & identified key if requested  $e5
  validating & identifying an incorrect self-desc JSON should return the validation errors in a Failure  $e6
  verifying & validating a correct self-desc JSON vs the correct schema should return the JSON in a Success  $e7
  verifying & validating a correct self-desc JSON vs the correct schema should return the JSON's data field in a Success  $e8
  verifying a correct self-desc JSON vs the incorrect schema should return an error message in a Failure  $e9
  verifying an incorrect self-desc JSON vs the incorrect schema should return an error message in a Failure  $e10
  """

  implicit val resolver = SpecHelpers.TestResolver

  val validJson = SpecHelpers.asJsonNode(
    """{"schema": "iglu:com.snowplowanalytics.iglu-test/stock-item/jsonschema/1-0-0", "data": { "id": "123-12", "name": "t-shirt", "price": 29.99 } }"""
  )

  def e1 = validJson.validate(false) must beSuccessful(validJson)

  def e2 = validJson.validate(true) must beSuccessful(validJson.get("data"))

  val invalidJson = SpecHelpers.asJsonNode(
    """{"schema": "iglu:com.snowplowanalytics.iglu-test/stock-item/jsonschema/1-0-0", "data": { "id": "123-12", "newName": "t-shirt", "price": "tbc" } }"""
  )
  val invalidExpected = NonEmptyList(
    SpecHelpers.asProcessingMessage(
      message =
        """object instance has properties which are not allowed by the schema: ["newName"]""",
      schema = """{"loadingURI":"#","pointer":""}""",
      instance = """{"pointer":""}""",
      keyword = "additionalProperties",
      foundExpected = None,
      requiredMissing = None,
      unwanted = Some("""["newName"]""")
    ),
    SpecHelpers.asProcessingMessage(
      message = """object has missing required properties (["name"])""",
      schema = """{"loadingURI":"#","pointer":""}""",
      instance = """{"pointer":""}""",
      keyword = "required",
      foundExpected = None,
      requiredMissing = Some("""["id","name","price"]""", """["name"]"""),
      unwanted = None
    )
  ).map(_.toString)

  def e3 = invalidJson.validate(false).leftMap(_.map(_.toString)) must beFailing(invalidExpected)

  val expectedKey =
    SchemaKey("com.snowplowanalytics.iglu-test", "stock-item", "jsonschema", "1-0-0")

  val expectedCriterion =
    SchemaCriterion("com.snowplowanalytics.iglu-test", "stock-item", "jsonschema", 1, 0, 0)

  def e4 = validJson.validateAndIdentifySchema(false) must beSuccessful((expectedKey, validJson))

  def e5 =
    validJson.validateAndIdentifySchema(true) must beSuccessful(
      (expectedKey, validJson.get("data")))

  def e6 =
    invalidJson.validateAndIdentifySchema(false).leftMap(_.map(_.toString)) must beFailing(
      invalidExpected)

  def e7 = validJson.verifySchemaAndValidate(expectedCriterion, false) must beSuccessful(validJson)

  def e8 =
    validJson.verifySchemaAndValidate(expectedCriterion, true) must beSuccessful(
      validJson.get("data"))

  val incorrectKey =
    SchemaCriterion("com.snowplowanalytics.iglu-test", "stock-item", "jsonschema", 2, 0, 0)
  val verifyExpected =
    "Verifying schema as iglu:com.snowplowanalytics.iglu-test/stock-item/jsonschema/2-0-0 failed: found iglu:com.snowplowanalytics.iglu-test/stock-item/jsonschema/1-0-0".toProcessingMessageNel
      .map(_.toString)

  def e9 =
    validJson
      .verifySchemaAndValidate(incorrectKey, false)
      .leftMap(_.map(_.toString)) must beFailing(verifyExpected)

  def e10 =
    invalidJson
      .verifySchemaAndValidate(incorrectKey, false)
      .leftMap(_.map(_.toString)) must beFailing(verifyExpected)
}
