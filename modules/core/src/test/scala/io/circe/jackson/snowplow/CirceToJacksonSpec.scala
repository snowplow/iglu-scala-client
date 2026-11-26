/*
 * Copyright (c) 2014-2026 Snowplow Analytics Ltd. All rights reserved.
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
package io.circe.jackson.snowplow

import java.math.{BigDecimal => JBigDecimal}

import scala.jdk.CollectionConverters._

// circe
import io.circe.Json
import io.circe.literal._

// Jackson
import com.fasterxml.jackson.databind.JsonNode
import com.fasterxml.jackson.databind.node._

// Specs2
import org.specs2.Specification

class CirceToJacksonSpec extends Specification {
  def is = s2"""

  This is a specification to test the conversion of circe Json to Jackson JsonNode

  convert a nested document containing every JSON type $e1
  keep the insertion order of an object with many fields $e2
  keep the order of array elements $e3
  accept a leaf inside an array at the depth limit and reject one level deeper $e4
  accept a leaf inside an object at the depth limit and reject one level deeper $e5
  reject a scalar when maxJsonDepth is 0 $e6
  reject an out-of-range number when validateNumberRange is true $e7
  report the first error found when an out-of-range number comes before a too-deep branch $e8
  report the first error found when a too-deep branch comes before an out-of-range number $e9
  convert an empty object and an empty array to empty nodes $e10
  """

  private val factory = JsonNodeFactory.instance

  def e1 = {
    // Numbers are built explicitly, because the representation circe-literal picks for a number
    // literal differs across Scala versions, and that decides the Jackson node type.
    val long    = Json.fromLong(42L)
    val decimal = Json.fromBigDecimal(BigDecimal("1.5"))
    val input =
      json"""{
        "null": null,
        "boolean": true,
        "string": "hello",
        "long": $long,
        "nested": {
          "array": [$decimal, false, "x", null, { "inner": [] }]
        }
      }"""

    val expected = factory.objectNode()
    expected.set[JsonNode]("null", NullNode.instance)
    expected.set[JsonNode]("boolean", BooleanNode.TRUE)
    expected.set[JsonNode]("string", TextNode.valueOf("hello"))
    expected.set[JsonNode]("long", LongNode.valueOf(42L))
    val inner = factory.objectNode()
    inner.set[JsonNode]("inner", factory.arrayNode())
    val array = factory.arrayNode()
    array.add(DecimalNode.valueOf(new JBigDecimal("1.5")))
    array.add(BooleanNode.FALSE)
    array.add(TextNode.valueOf("x"))
    array.add(NullNode.instance)
    array.add(inner)
    val nested = factory.objectNode()
    nested.set[JsonNode]("array", array)
    expected.set[JsonNode]("nested", nested)

    circeToJackson(input, Int.MaxValue) must beRight(expected: JsonNode)
  }

  def e2 = {
    val input = json"""{ "z": 1, "y": 2, "x": 3, "w": 4, "v": 5, "u": 6, "t": 7, "s": 8 }"""
    circeToJackson(input, Int.MaxValue).map(_.fieldNames().asScala.toList) must beRight(
      List("z", "y", "x", "w", "v", "u", "t", "s")
    )
  }

  def e3 = {
    val input = json"""["c", "a", "b", "e", "d"]"""
    circeToJackson(input, Int.MaxValue).map(_.elements().asScala.map(_.asText).toList) must beRight(
      List("c", "a", "b", "e", "d")
    )
  }

  def e4 = {
    val input = json"""[[["leaf"]]]"""
    (circeToJackson(input, 4) must beRight) and
      (circeToJackson(input, 3) must beLeft(CirceToJsonError.MaxDepthExceeded: CirceToJsonError))
  }

  def e5 = {
    val input = json"""{ "a": { "b": { "c": "leaf" } } }"""
    (circeToJackson(input, 4) must beRight) and
      (circeToJackson(input, 3) must beLeft(CirceToJsonError.MaxDepthExceeded: CirceToJsonError))
  }

  def e6 =
    circeToJackson(Json.fromString("scalar"), 0) must beLeft(
      CirceToJsonError.MaxDepthExceeded: CirceToJsonError
    )

  private val outOfRange = Json.fromBigDecimal(BigDecimal("1e400"))

  def e7 =
    circeToJackson(outOfRange, Int.MaxValue, validateNumberRange = true) must beLeft(
      CirceToJsonError.NumberOutOfRange("1E+400"): CirceToJsonError
    )

  def e8 = {
    val input = json"""{ "number": $outOfRange, "deep": [[["leaf"]]] }"""
    circeToJackson(input, 3, validateNumberRange = true) must beLeft(
      CirceToJsonError.NumberOutOfRange("1E+400"): CirceToJsonError
    )
  }

  def e9 = {
    val input = json"""{ "deep": [[["leaf"]]], "number": $outOfRange }"""
    circeToJackson(input, 3, validateNumberRange = true) must beLeft(
      CirceToJsonError.MaxDepthExceeded: CirceToJsonError
    )
  }

  def e10 =
    (circeToJackson(json"""{}""", Int.MaxValue) must beRight(factory.objectNode(): JsonNode)) and
      (circeToJackson(json"""[]""", Int.MaxValue) must beRight(factory.arrayNode(): JsonNode))
}
