/*
 * Copyright (c) 2014-2020 Snowplow Analytics Ltd. All rights reserved.
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
package io.circe
package jackson

import scala.jdk.CollectionConverters._

import java.math.{BigDecimal => JBigDecimal}

import com.fasterxml.jackson.core.JsonParseException

import com.fasterxml.jackson.databind.{JsonNode, ObjectMapper}
import com.fasterxml.jackson.databind.node._

/** A hack to overcome https://github.com/circe/circe-jackson/issues/65 */
package object snowplow {
  private val mapper                 = new ObjectMapper
  private val negativeZeroJson: Json = Json.fromDoubleOrNull(-0.0)

  /**
   * Converts given circe's Json instance to Jackson's JsonNode
   * Numbers with exponents exceeding Integer.MAX_VALUE are converted to strings
   * '''Warning: This implementation is not stack safe and will fail on very deep structures'''
   * @param json instance of circe's Json
   * @return converted JsonNode
   */
  final def circeToJackson(json: Json): JsonNode =
    json.fold(
      NullNode.instance,
      BooleanNode.valueOf(_),
      number =>
        if (json == negativeZeroJson)
          DoubleNode.valueOf(number.toDouble)
        else
          number match {
            case _: JsonBiggerDecimal | _: JsonBigDecimal =>
              number.toBigDecimal
                .map(bigDecimal => DecimalNode.valueOf(bigDecimal.underlying))
                .getOrElse(TextNode.valueOf(number.toString))
            case JsonLong(x)   => LongNode.valueOf(x)
            case JsonDouble(x) => DoubleNode.valueOf(x)
            case JsonFloat(x)  => FloatNode.valueOf(x)
            case JsonDecimal(x) =>
              try if (x.contains('.') || x.toLowerCase.contains('e'))
                DecimalNode.valueOf(new JBigDecimal(x))
              else
                getJsonNodeFromStringContent(x)
              catch {
                case _: NumberFormatException => TextNode.valueOf(x)
                case _: JsonParseException    => TextNode.valueOf(x)
              }
          },
      s => TextNode.valueOf(s),
      array => JsonNodeFactory.instance.arrayNode.addAll(array.map(circeToJackson).asJava),
      obj =>
        objectNodeSetAll(
          JsonNodeFactory.instance.objectNode,
          obj.toMap.map {
            case (k, v) => (k, circeToJackson(v))
          }.asJava
        )
    )

  def objectNodeSetAll(node: ObjectNode, fields: java.util.Map[String, JsonNode]): JsonNode =
    node.setAll[JsonNode](fields)

  private def getJsonNodeFromStringContent(content: String): JsonNode =
    mapper.readTree(content)
}
