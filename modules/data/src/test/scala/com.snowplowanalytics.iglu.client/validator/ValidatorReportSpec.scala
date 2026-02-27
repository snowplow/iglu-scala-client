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

import io.circe.syntax._
import io.circe.literal._

import org.specs2.Specification

class ValidatorReportSpec extends Specification {
  def is = s2"""
  ValidatorReport should
    encode to JSON with all fields              $e1
    encode to JSON with None fields             $e2
    decode from JSON with all fields            $e3
    decode from JSON with null optional fields  $e4
    decode from JSON with missing targets field $e5
    roundtrip encode/decode                     $e6
  """

  def e1 = {
    val report = ValidatorReport("error msg", Some("$.path"), List("t1", "t2"), Some("type"))
    val json = json"""{
      "message": "error msg",
      "path": "$$.path",
      "keyword": "type",
      "targets": ["t1", "t2"]
    }"""
    report.asJson must beEqualTo(json)
  }

  def e2 = {
    val report = ValidatorReport("error msg", None, Nil, None)
    val json = json"""{
      "message": "error msg",
      "path": null,
      "keyword": null,
      "targets": []
    }"""
    report.asJson must beEqualTo(json)
  }

  def e3 = {
    val json = json"""{
      "message": "some error",
      "path": "$$.field",
      "keyword": "required",
      "targets": ["a"]
    }"""
    val expected = ValidatorReport("some error", Some("$.field"), List("a"), Some("required"))
    json.as[ValidatorReport] must beRight(expected)
  }

  def e4 = {
    val json = json"""{
      "message": "some error",
      "path": null,
      "keyword": null,
      "targets": []
    }"""
    val expected = ValidatorReport("some error", None, Nil, None)
    json.as[ValidatorReport] must beRight(expected)
  }

  def e5 = {
    val json = json"""{
      "message": "some error",
      "path": null,
      "keyword": null
    }"""
    val expected = ValidatorReport("some error", None, Nil, None)
    json.as[ValidatorReport] must beRight(expected)
  }

  def e6 = {
    val report = ValidatorReport("roundtrip test", Some("$.x"), List("y"), Some("kw"))
    report.asJson.as[ValidatorReport] must beRight(report)
  }
}
