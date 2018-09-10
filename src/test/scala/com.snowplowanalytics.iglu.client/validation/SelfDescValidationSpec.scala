/*
 * Copyright (c) 2014-2019 Snowplow Analytics Ltd. All rights reserved.
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

  val validJson =
    json"""{"schema": "iglu:com.snowplowanalytics.iglu-test/stock-item/jsonschema/1-0-0", "data": { "id": "123-12", "name": "t-shirt", "price": 29.99 } }"""

  val validJsonExpectedData = validJson.hcursor
    .get[Json]("data")

  def e1 = {
    val action = for {
      resolver <- SpecHelpers.TestResolver
      result   <- validJson.validate[IO](resolver, dataOnly = false)
    } yield result must beRight(validJson)

    action.unsafeRunSync()
  }

  def e2 = {
    val action = for {
      resolver <- SpecHelpers.TestResolver
      result   <- validJson.validate[IO](resolver, dataOnly = true)
    } yield result must beEqualTo(validJsonExpectedData)

    action.unsafeRunSync()
  }

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

  def e3 = {
    val action = for {
      resolver <- SpecHelpers.TestResolver
      result   <- invalidJson.validate[IO](resolver, dataOnly = false)
    } yield result must beLeft

    action.unsafeRunSync()
  }

  val expectedKey =
    SchemaKey(
      "com.snowplowanalytics.iglu-test",
      "stock-item",
      "jsonschema",
      SchemaVer.Full(1, 0, 0))

  val expectedCriterion =
    SchemaCriterion("com.snowplowanalytics.iglu-test", "stock-item", "jsonschema", 1, 0, 0)

  def e4 = {
    val action = for {
      resolver <- SpecHelpers.TestResolver
      result   <- validJson.validateAndIdentifySchema[IO](resolver, dataOnly = false)
    } yield result must beRight((expectedKey, validJson))

    action.unsafeRunSync()
  }

  def e5 = {
    val action = for {
      resolver <- SpecHelpers.TestResolver
      result   <- validJson.validateAndIdentifySchema[IO](resolver, dataOnly = true)
    } yield result must beEqualTo(validJsonExpectedData.map(data => (expectedKey, data)))

    action.unsafeRunSync()
  }

  def e6 = {
    val action = for {
      resolver <- SpecHelpers.TestResolver
      result   <- invalidJson.validateAndIdentifySchema[IO](resolver, dataOnly = false)
    } yield result must beLeft

    action.unsafeRunSync()
  }

  def e7 = {
    val action = for {
      resolver <- SpecHelpers.TestResolver
      result   <- validJson.verifySchemaAndValidate[IO](resolver, expectedCriterion, dataOnly = false)
    } yield result must beRight(validJson)

    action.unsafeRunSync()
  }

  def e8 = {
    val action = for {
      resolver <- SpecHelpers.TestResolver
      result   <- validJson.verifySchemaAndValidate[IO](resolver, expectedCriterion, dataOnly = true)
    } yield result must beEqualTo(validJsonExpectedData)

    action.unsafeRunSync()
  }

  val incorrectKey =
    SchemaCriterion("com.snowplowanalytics.iglu-test", "stock-item", "jsonschema", 2, 0, 0)
  val verifyExpected =
    NonEmptyList.one(ProcessingMessage(
      "Verifying schema as iglu:com.snowplowanalytics.iglu-test/stock-item/jsonschema/2-0-0 failed: found iglu:com.snowplowanalytics.iglu-test/stock-item/jsonschema/1-0-0").toString)

  def e9 = {
    val action = for {
      resolver <- SpecHelpers.TestResolver
      result   <- validJson.verifySchemaAndValidate[IO](resolver, incorrectKey, dataOnly = false)
    } yield result must beLeft

    action.unsafeRunSync()
  }

  def e10 = {
    val action = for {
      resolver <- SpecHelpers.TestResolver
      result   <- invalidJson.verifySchemaAndValidate[IO](resolver, incorrectKey, dataOnly = false)
      resultStr = result.leftMap(_.map(_.toString))
    } yield resultStr must beLeft(verifyExpected)

    action.unsafeRunSync()
  }
}
