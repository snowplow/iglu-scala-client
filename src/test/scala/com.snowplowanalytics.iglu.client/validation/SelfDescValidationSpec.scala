/*
 * Copyright (c) 2014-2018 Snowplow Analytics Ltd. All rights reserved.
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

// Cats
import cats.syntax.either._
import cats.data.NonEmptyList
import cats.effect.IO

// circe
import io.circe.Json
import io.circe.literal._

// Iglu Core
import com.snowplowanalytics.iglu.core.{SchemaCriterion, SchemaKey, SchemaVer}

// This project
import ValidatableCirceMethods._
import validation.ProcessingMessageMethods._

// Specs2
import org.specs2.Specification
import org.specs2.matcher.ValidatedMatchers

class SelfDescValidationSpec extends Specification with ValidatedMatchers {
  def is = s2"""

  This is a specification to test validation of self-describing Jsons

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

  implicit val resolver = SpecHelpers.TestResolver.unsafeRunSync()

  val validJson =
    json"""{"schema": "iglu:com.snowplowanalytics.iglu-test/stock-item/jsonschema/1-0-0", "data": { "id": "123-12", "name": "t-shirt", "price": 29.99 } }"""

  val validJsonExpectedData = validJson.hcursor
    .get[Json]("data")

  def e1 = validJson.validate[IO](dataOnly = false).map(_ must beRight(validJson)).unsafeRunSync()

  def e2 =
    validJson
      .validate[IO](dataOnly = true)
      .map(_ must beEqualTo(validJsonExpectedData))
      .unsafeRunSync()

  val invalidJson =
    json"""{"schema": "iglu:com.snowplowanalytics.iglu-test/stock-item/jsonschema/1-0-0", "data": { "id": "123-12", "newName": "t-shirt", "price": "tbc" } }"""

  val invalidExpected = NonEmptyList
    .of(
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
    )
    .map(_.toString)

  def e3 =
    invalidJson
      .validate[IO](dataOnly = false)
      .map(_.leftMap(_.map(_.toString)))
      .map(_ must beLeft) // TODO: check expected messages
      .unsafeRunSync()

  val expectedKey =
    SchemaKey(
      "com.snowplowanalytics.iglu-test",
      "stock-item",
      "jsonschema",
      SchemaVer.Full(1, 0, 0))

  val expectedCriterion =
    SchemaCriterion("com.snowplowanalytics.iglu-test", "stock-item", "jsonschema", 1, 0, 0)

  def e4 =
    validJson
      .validateAndIdentifySchema[IO](dataOnly = false)
      .map(_ must beRight((expectedKey, validJson)))
      .unsafeRunSync()

  def e5 =
    validJson
      .validateAndIdentifySchema[IO](dataOnly = true)
      .map(_ must beEqualTo(validJsonExpectedData.map(data => (expectedKey, data))))
      .unsafeRunSync()

  def e6 =
    invalidJson
      .validateAndIdentifySchema[IO](dataOnly = false)
      .map(_.leftMap(_.map(_.toString)))
      .map(_ must beLeft) // TODO: Check expected messages
      .unsafeRunSync()

  def e7 =
    validJson
      .verifySchemaAndValidate[IO](expectedCriterion, dataOnly = false)
      .map(_ must beRight(validJson))
      .unsafeRunSync()

  def e8 =
    validJson
      .verifySchemaAndValidate[IO](expectedCriterion, dataOnly = true)
      .map(_ must beEqualTo(validJsonExpectedData))
      .unsafeRunSync()

  val incorrectKey =
    SchemaCriterion("com.snowplowanalytics.iglu-test", "stock-item", "jsonschema", 2, 0, 0)
  val verifyExpected =
    "Verifying schema as iglu:com.snowplowanalytics.iglu-test/stock-item/jsonschema/2-0-0 failed: found iglu:com.snowplowanalytics.iglu-test/stock-item/jsonschema/1-0-0".toProcessingMessageNel
      .map(_.toString)

  def e9 =
    validJson
      .verifySchemaAndValidate[IO](incorrectKey, dataOnly = false)
      .map(_.leftMap(_.map(_.toString)))
      .map(_ must beLeft(verifyExpected))
      .unsafeRunSync()

  def e10 =
    invalidJson
      .verifySchemaAndValidate[IO](incorrectKey, dataOnly = false)
      .map(_.leftMap(_.map(_.toString)))
      .map(_ must beLeft(verifyExpected))
      .unsafeRunSync()
}
