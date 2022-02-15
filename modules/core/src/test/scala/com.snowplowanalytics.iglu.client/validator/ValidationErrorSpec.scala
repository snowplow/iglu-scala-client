/*
 * Copyright (c) 2014-2022 Snowplow Analytics Ltd. All rights reserved.
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

import org.specs2.Specification

class ValidationErrorSpec extends Specification {
  def is = s2"""
  Encode and decode ValidationError $e1
  """

  def e1 = {
    val error: ValidatorError =
      ValidatorError.InvalidData(
        NonEmptyList.of(
          ValidatorReport("Something went wrong", Some("$.path"), Nil, Some("type")),
          ValidatorReport("Something went wrong again", None, Nil, None),
          ValidatorReport("Something went wrong with targets", None, List("type", "property"), None)
        )
      )

    val json = json"""{
      "dataReports" : [
        {
          "message" : "Something went wrong",
          "path" : "$$.path",
          "keyword" : "type",
          "targets" : [
          ]
        },
        {
          "message" : "Something went wrong again",
          "path" : null,
          "keyword" : null,
          "targets" : [
          ]
        },
        {
          "message" : "Something went wrong with targets",
          "path" : null,
          "keyword" : null,
          "targets" : [
            "type",
            "property"
          ]
        }
      ]
    }"""

    val encoding = error.asJson must beEqualTo(json)
    val decoding = json.as[ValidatorError] must beRight(error)

    encoding and decoding
  }
}
