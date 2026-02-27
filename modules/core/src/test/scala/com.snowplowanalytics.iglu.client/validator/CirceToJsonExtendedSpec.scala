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

import io.circe.Json
import io.circe.jackson.snowplow.circeToJackson
import io.circe.parser.parse

import org.specs2.Specification

class CirceToJsonExtendedSpec extends Specification {
  def is = s2"""
  circeToJackson number handling should
    convert negative zero                                 $negativeZero
    convert BigDecimal that is valid Int                  $bigDecimalInt
    convert BigDecimal that is valid Long                 $bigDecimalLong
    convert BigDecimal that is whole but big              $bigDecimalBigInt
    convert BigDecimal that is fractional                 $bigDecimalFractional
    convert JsonLong value                                $jsonLong
    convert JsonDouble value                              $jsonDouble
    convert JsonFloat value                               $jsonFloat
    convert number with decimal point via parser          $numberWithDot
    convert number with exponent via parser               $numberWithExp
    convert integer string via parser                     $integerViaParser
    handle empty array                                    $emptyArray
    handle empty object                                   $emptyObject
    fail when depth 0 and json is non-trivial             $depthZeroNonTrivial
    succeed when depth is 2 for flat json                  $depthExactlyOne
    handle array depth exhaustion                         $arrayDepthExhaustion
    handle object depth exhaustion                        $objectDepthExhaustion
    convert deeply nested but valid JSON                  $deeplyNestedValid
    handle mixed array with various types                 $mixedArray
    handle object with various value types                $mixedObject
  """

  def negativeZero = {
    val json   = Json.fromDoubleOrNull(-0.0)
    val result = circeToJackson(json, 10)
    result must beRight
  }

  def bigDecimalInt = {
    val json   = Json.fromBigDecimal(BigDecimal(42))
    val result = circeToJackson(json, 10)
    result must beRight.which(_.isInt)
  }

  def bigDecimalLong = {
    val json   = Json.fromBigDecimal(BigDecimal(3000000000L))
    val result = circeToJackson(json, 10)
    result must beRight.which(_.isLong)
  }

  def bigDecimalBigInt = {
    val big    = BigDecimal("99999999999999999999")
    val json   = Json.fromBigDecimal(big)
    val result = circeToJackson(json, 10)
    result must beRight.which(_.isBigInteger)
  }

  def bigDecimalFractional = {
    val json   = Json.fromBigDecimal(BigDecimal("1.23456789"))
    val result = circeToJackson(json, 10)
    result must beRight.which(_.isBigDecimal)
  }

  def jsonLong = {
    val json   = Json.fromLong(Long.MaxValue)
    val result = circeToJackson(json, 10)
    result must beRight.which(_.isLong)
  }

  def jsonDouble = {
    val json   = Json.fromDoubleOrNull(2.718281828)
    val result = circeToJackson(json, 10)
    result must beRight.which(_.isDouble)
  }

  def jsonFloat = {
    val json = Json.fromFloat(1.5f)
    json must beSome[Json].which { j =>
      circeToJackson(j, 10).isRight
    }
  }

  def numberWithDot = {
    // Parsing "3.14" creates JsonDecimal("3.14") which has a dot
    val json   = parse("3.14").toOption.get
    val result = circeToJackson(json, 10)
    result must beRight
  }

  def numberWithExp = {
    // Parsing "1.5e2" creates JsonDecimal("1.5e2") which has an exponent
    val json   = parse("1.5e2").toOption.get
    val result = circeToJackson(json, 10)
    result must beRight
  }

  def integerViaParser = {
    // Parsing "12345" creates a JsonDecimal/JsonLong with no dot or exponent
    val json   = parse("12345").toOption.get
    val result = circeToJackson(json, 10)
    result must beRight
  }

  def emptyArray = {
    val json   = Json.arr()
    val result = circeToJackson(json, 10)
    result must beRight.which(_.isArray)
  }

  def emptyObject = {
    val json   = Json.obj()
    val result = circeToJackson(json, 10)
    result must beRight.which(_.isObject)
  }

  def depthZeroNonTrivial = {
    val json   = Json.obj("a" -> Json.True)
    val result = circeToJackson(json, 0)
    result must beLeft
  }

  def depthExactlyOne = {
    // Depth 1 passes the initial check but recursive calls use depth-1=0,
    // so even primitives inside containers fail. Flat objects need depth >= 2.
    val json   = Json.obj("a" -> Json.fromInt(1))
    val result = circeToJackson(json, 2)
    result must beRight
  }

  def arrayDepthExhaustion = {
    val nested = Json.arr(Json.obj("a" -> Json.True))
    val result = circeToJackson(nested, 1)
    result must beLeft
  }

  def objectDepthExhaustion = {
    val nested = Json.obj("a" -> Json.obj("b" -> Json.True))
    val result = circeToJackson(nested, 1)
    result must beLeft
  }

  def deeplyNestedValid = {
    // 5 levels of nesting, depth 10 should be fine
    val json   = parse("""{"a":{"b":{"c":{"d":{"e":"leaf"}}}}}""").toOption.get
    val result = circeToJackson(json, 10)
    result must beRight
  }

  def mixedArray = {
    val json = Json.arr(
      Json.Null,
      Json.True,
      Json.False,
      Json.fromString("str"),
      Json.fromInt(1),
      Json.fromDoubleOrNull(2.5),
      Json.obj("key" -> Json.fromString("val"))
    )
    val result = circeToJackson(json, 10)
    result must beRight.which(_.size() == 7)
  }

  def mixedObject = {
    val json = Json.obj(
      "null"   -> Json.Null,
      "bool"   -> Json.True,
      "string" -> Json.fromString("hello"),
      "int"    -> Json.fromInt(42),
      "double" -> Json.fromDoubleOrNull(3.14),
      "array"  -> Json.arr(Json.fromInt(1)),
      "nested" -> Json.obj("inner" -> Json.True)
    )
    val result = circeToJackson(json, 10)
    result must beRight.which(_.size() == 7)
  }
}
