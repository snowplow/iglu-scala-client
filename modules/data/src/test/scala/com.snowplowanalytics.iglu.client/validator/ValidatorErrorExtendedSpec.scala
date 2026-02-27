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

import cats.data.NonEmptyList

import io.circe.syntax._
import io.circe.literal._

import com.snowplowanalytics.iglu.client.ClientError

import org.specs2.Specification

class ValidatorErrorExtendedSpec extends Specification {
  def is = s2"""
  ValidatorError should
    encode InvalidSchema to JSON                $e1
    decode InvalidSchema from JSON              $e2
    roundtrip InvalidSchema                     $e3
    toClientError should wrap in ValidationError $e4
    SchemaIssue encode and decode               $e5
    schemaIssue helper should create from Throwable $e6
  """

  def e1 = {
    val error: ValidatorError =
      ValidatorError.InvalidSchema(
        NonEmptyList.of(
          ValidatorError.SchemaIssue("$.properties.name", "type is missing"),
          ValidatorError.SchemaIssue("$.required", "should be array")
        )
      )

    val json = json"""{
      "schemaIssues": [
        {"path": "$$.properties.name", "message": "type is missing"},
        {"path": "$$.required", "message": "should be array"}
      ]
    }"""

    error.asJson must beEqualTo(json)
  }

  def e2 = {
    val json = json"""{
      "schemaIssues": [
        {"path": "$$", "message": "issue found"}
      ]
    }"""
    val expected: ValidatorError =
      ValidatorError.InvalidSchema(
        NonEmptyList.of(ValidatorError.SchemaIssue("$", "issue found"))
      )
    json.as[ValidatorError] must beRight(expected)
  }

  def e3 = {
    val error: ValidatorError =
      ValidatorError.InvalidSchema(
        NonEmptyList.of(ValidatorError.SchemaIssue("$.x", "bad"))
      )
    error.asJson.as[ValidatorError] must beRight(error)
  }

  def e4 = {
    val error: ValidatorError =
      ValidatorError.InvalidData(
        NonEmptyList.of(ValidatorReport("msg", None, Nil, None))
      )
    val clientError = error.toClientError(Some("1-0-1"))
    clientError must beAnInstanceOf[ClientError.ValidationError]
    val ve = clientError.asInstanceOf[ClientError.ValidationError]
    (ve.error must_== error) and (ve.supersededBy must_== Some("1-0-1"))
  }

  def e5 = {
    val issue    = ValidatorError.SchemaIssue("$.type", "wrong type")
    val json     = json"""{"path": "$$.type", "message": "wrong type"}"""
    val encoding = issue.asJson must beEqualTo(json)
    val decoding = json.as[ValidatorError.SchemaIssue] must beRight(issue)
    encoding and decoding
  }

  def e6 = {
    val exception = new RuntimeException("test exception message")
    val result    = ValidatorError.schemaIssue(exception)
    val issue     = result.issues.head
    (issue.path must_== "$") and
      (issue.message must_== "test exception message")
  }
}
