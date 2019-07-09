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

import cats.data.NonEmptyList

import io.circe.syntax._
import io.circe.literal._

import com.snowplowanalytics.iglu.client.resolver.LookupHistory
import com.snowplowanalytics.iglu.client.resolver.registries.RegistryError
import com.snowplowanalytics.iglu.client.validator.{ValidatorError, ValidatorReport}

import org.specs2.Specification

class ClientErrorSpec extends Specification {
  def is =
    s2"""
  Encode and decode ResolutionError $e1
  Encode and decode ValidationError $e2
  """

  def e1 = {
    val error: ClientError = ClientError.ResolutionError(
      Map(
        "First repo" -> LookupHistory(
          Set(RegistryError.NotFound, RegistryError.RepoFailure("Server outage")),
          1,
          SpecHelpers.now),
        "Second repo" -> LookupHistory(
          Set(RegistryError.ClientFailure("Internal exception")),
          1,
          SpecHelpers.now.plusMillis(100))
      ))

    val json =
      json"""{
        "error" : "ResolutionError",
        "lookupHistory" : [
          {
            "repository" : "First repo",
            "errors" : [
              {
                "error" : "NotFound"
              },
              {
                "error" : "RepoFailure",
                "message" : "Server outage"
              }
            ],
            "attempts" : 1,
            "lastAttempt" : "2019-07-08T15:04:45Z"
          },
          {
            "repository" : "Second repo",
            "errors" : [
              {
                "error" : "ClientFailure",
                "message" : "Internal exception"
              }
            ],
            "attempts" : 1,
            "lastAttempt" : "2019-07-08T15:04:45.100Z"
          }
        ]
      }"""

    val encoding = error.asJson must beEqualTo(json)
    val decoding = json.as[ClientError] must beRight(error)

    encoding and decoding
  }

  def e2 = {
    val error: ClientError = ClientError.ValidationError(
      ValidatorError.InvalidData(
        NonEmptyList.of(
          ValidatorReport("Something went wrong", Some("$.path"), Nil, Some("type")),
          ValidatorReport("Something went wrong again", None, Nil, None),
          ValidatorReport("Something went wrong with targets", None, List("type", "property"), None)
        ))
    )

    val json =
      json"""{
        "error" : "ValidationError",
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
    val decoding = json.as[ClientError] must beRight(error)

    encoding and decoding
  }
}
