/*
 * Copyright (c) 2012-2014 Snowplow Analytics Ltd. All rights reserved.
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

// Jackson
import com.github.fge.jackson.JsonLoader

// Scalaz
import scalaz._
import Scalaz._

// Specs2
import org.specs2.Specification
import org.specs2.matcher.DataTables
import org.specs2.scalaz.ValidationMatchers

class RawValidationSpec extends Specification with DataTables with ValidationMatchers { def is =

  "This is a specification to test the basic ValidatableJsonNode functionality"         ^
                                                                                       p^
  "a JsonNode should be pimped to a ValidatableJsonNode as needed"                      ! e1^
  "JsonNodes that pass explicit validation should be wrapped in a Success"              ! e2^
  "JsonNodes that fail explicit validation should wrap ProcessageMessages in a Failure" ! e3^  
                                                                                        end

  val SimpleSchema = JsonLoader.fromResource("/raw-jsonschema/beer-schema.json")

  import ValidatableJsonMethods._
  implicit val resolver = Bootstrap.Resolver

  def e1 = {
    val json = ValidationSpecHelpers.asJsonNode("""{"country": "JP", "beers": ["Asahi", "Orion", "..."]}""")
    json.validateAgainstSchema(SimpleSchema) must beSuccessful(json)
  }

  def e2 =
    foreach(Seq(
      """{"country": "AQ", "beers": []}""",
      """{"country":"GR","beers":["Fix","Mythos"]}""",
      """{"country": "fr","beers": ["Jenlain"]}"""
    )) {
      str: String => {
        val json = ValidationSpecHelpers.asJsonNode(str)
        json.validateAgainstSchema(SimpleSchema) must beSuccessful(json)
      }
    }

  def e3 =
    "SPEC NAME"          || "IN JSON"                                        | "OUT MESSAGE"                                                                                 | "OUT SCHEMA"                                                 | "OUT INSTANCE"               | "OUT KEYWORD" | "OUT FOUND & EXPECTED"              | "OUT REQUIRED & MISSING"                           |
    "numeric country"    !! """{"country": 123, "beers": []}"""              ! """instance type (integer) does not match any allowed primitive type (allowed: ["string"])""" ! """{"loadingURI":"#","pointer":"/properties/country"}"""     ! """{"pointer":"/country"}""" ! "type"        ! Some(("integer", """["string"]""")) ! None                                               |
    "missing beers"      !! """{"country": "cy"}"""                          ! """object has missing required properties (["beers"])"""                                      ! """{"loadingURI":"#","pointer":""}"""                        ! """{"pointer":""}"""         ! "required"    ! None                                ! Some(("""["beers","country"]""", """["beers"]""")) |
    "heterogenous beers" !! """{"country": "GB", "beers": ["ale", false]}""" ! """instance type (boolean) does not match any allowed primitive type (allowed: ["string"])""" ! """{"loadingURI":"#","pointer":"/properties/beers/items"}""" ! """{"pointer":"/beers/1"}""" ! "type"        ! Some(("boolean", """["string"]""")) ! None                                               |> {

      (_, input, message, schema, instance, keyword, foundExpected, requiredMissing) => {
        val json = ValidationSpecHelpers.asJsonNode(input)
        json.validateAgainstSchema(SimpleSchema) must beLike {
          case Failure(NonEmptyList(head, tail @ _*)) if tail.isEmpty =>
            head.toString must_== ValidationSpecHelpers.asProcessingMessage(message, schema, instance, keyword, foundExpected, requiredMissing).toString
        }
      }
    }

}
