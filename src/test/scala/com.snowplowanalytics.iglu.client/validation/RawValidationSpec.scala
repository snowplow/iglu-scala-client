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
import cats._
import cats.implicits._
import cats.data.NonEmptyList
import cats.effect.IO

// circe
import io.circe.literal._
import io.circe.parser.parse

// Specs2
import org.specs2.Specification
import org.specs2.matcher.DataTables

class RawValidationSpec extends Specification with DataTables {
  def is = s2"""

  This is a specification to test the basic Validatable functionality

  a Json should be pimped to a ValidatableJson as needed  $e1
  JsonS that pass explicit validation should be wrapped in a Success  $e2
  Jsons that fail explicit validation should wrap ProcessageMessages in a Failure  $e3
  """

  val simpleSchemaResult =
    parse(
      scala.io.Source
        .fromInputStream(getClass.getResourceAsStream("/raw-jsonschema/beer-schema.json"))
        .mkString).leftMap(_ => ProcessingMessage("foo"))

  import ValidatableCirceMethods._

  def e1 = {
    val json = json"""{"country": "JP", "beers": ["Asahi", "Orion", "..."]}"""

    val result =
      simpleSchemaResult.flatMap(simpleSchema => json.validateAgainstSchema(simpleSchema))

    result must beRight(json)
  }

  def e2 =
    foreach(
      List(
        json"""{"country": "AQ", "beers": []}""",
        json"""{"country":"GR","beers":["Fix","Mythos"]}""",
        json"""{"country": "fr","beers": ["Jenlain"]}"""
      )) { json =>
      val result =
        simpleSchemaResult.flatMap(simpleSchema => json.validateAgainstSchema(simpleSchema))

      result must beRight(json)
    }

  def e3 =
    "SPEC NAME" || "IN JSON" | "OUT MESSAGE" | "OUT SCHEMA" | "OUT INSTANCE" | "OUT KEYWORD" | "OUT FOUND & EXPECTED" | "OUT REQUIRED & MISSING" |
      "numeric country" !! json"""{"country": 123, "beers": []}""" ! """instance type (integer) does not match any allowed primitive type (allowed: ["string"])""" ! """{"loadingURI":"#","pointer":"/properties/country"}""" ! """{"pointer":"/country"}""" ! "type" ! Some(
        ("integer", """["string"]""")) ! None |
      "missing beers" !! json"""{"country": "cy"}""" ! """object has missing required properties (["beers"])""" ! """{"loadingURI":"#","pointer":""}""" ! """{"pointer":""}""" ! "required" ! None ! Some(
        ("""["beers","country"]""", """["beers"]""")) |
      "heterogenous beers" !! json"""{"country": "GB", "beers": ["ale", false]}""" ! """instance type (boolean) does not match any allowed primitive type (allowed: ["string"])""" ! """{"loadingURI":"#","pointer":"/properties/beers/items"}""" ! """{"pointer":"/beers/1"}""" ! "type" ! Some(
        ("boolean", """["string"]""")) ! None |> {

      (_, input, message, schema, instance, keyword, foundExpected, requiredMissing) =>
        {
          val result =
            simpleSchemaResult.flatMap(simpleSchema => input.validateAgainstSchema(simpleSchema))

          result must beLeft
        }
    }

}
