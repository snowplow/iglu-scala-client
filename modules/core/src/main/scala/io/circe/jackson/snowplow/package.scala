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
package io.circe
package jackson

import cats.syntax.either._
import cats.syntax.eq._
import cats.syntax.traverse._

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
   * @param json instance of circe's Json
   * @return converted JsonNode
   */
  final def circeToJackson(json: Json, maxJsonDepth: Int): Either[CirceToJsonError, JsonNode] =
    circeToJackson(json, maxJsonDepth, validateNumberRange = false)

  /**
   * Converts given circe's Json instance to Jackson's JsonNode
   * Numbers with exponents exceeding Integer.MAX_VALUE are converted to strings
   * @param json instance of circe's Json
   * @param validateNumberRange whether to reject numbers whose magnitude lies outside the range of
   *   a 64-bit floating point number. Appropriate for instance data, which downstream consumers
   *   read as doubles, but not for schemas, where a number is a bound rather than a value to be
   *   loaded.
   * @return converted JsonNode
   */
  final def circeToJackson(
    json: Json,
    maxJsonDepth: Int,
    validateNumberRange: Boolean
  ): Either[CirceToJsonError, JsonNode] =
    if (maxJsonDepth <= 0) CirceToJsonError.MaxDepthExceeded.asLeft
    else
      json.fold(
        NullNode.instance.asRight,
        BooleanNode.valueOf(_).asRight,
        number =>
          (if (validateNumberRange) checkMagnitudeInDoubleRange(number) else ().asRight).map { _ =>
            if (json == negativeZeroJson)
              DoubleNode.valueOf(number.toDouble)
            else
              number match {
                case _: JsonBiggerDecimal | _: JsonBigDecimal =>
                  number.toBigDecimal
                    .map(bigDecimal => {
                      if (bigDecimal.isValidInt)
                        IntNode.valueOf(bigDecimal.intValue)
                      else if (bigDecimal.isValidLong) {
                        LongNode.valueOf(bigDecimal.longValue)
                      } else if (bigDecimal.isWhole) {
                        BigIntegerNode.valueOf(bigDecimal.toBigInt.underlying)
                      } else
                        DecimalNode.valueOf(bigDecimal.underlying)
                    })
                    .getOrElse(TextNode.valueOf(number.toString))
                case JsonLong(x)   => LongNode.valueOf(x)
                case JsonDouble(x) => DoubleNode.valueOf(x)
                case JsonFloat(x)  => FloatNode.valueOf(x)
                case JsonDecimal(x) =>
                  try
                    if (x.contains('.') || x.toLowerCase.contains('e'))
                      DecimalNode.valueOf(new JBigDecimal(x))
                    else
                      getJsonNodeFromStringContent(x)
                  catch {
                    case _: NumberFormatException => TextNode.valueOf(x)
                    case _: JsonParseException    => TextNode.valueOf(x)
                  }
              }
          },
        s => TextNode.valueOf(s).asRight,
        array =>
          array
            .traverse(circeToJackson(_, maxJsonDepth - 1, validateNumberRange))
            .map(l => JsonNodeFactory.instance.arrayNode.addAll(l.asJava)),
        obj =>
          obj.toList
            .traverse { case (k, v) =>
              circeToJackson(v, maxJsonDepth - 1, validateNumberRange).map((k, _))
            }
            .map { l =>
              objectNodeSetAll(
                JsonNodeFactory.instance.objectNode,
                l.toMap.asJava
              )
            }
      )

  private def checkMagnitudeInDoubleRange(number: JsonNumber): Either[CirceToJsonError, Unit] = {
    val magnitude = Math.abs(number.toDouble)
    // A NaN or an infinity satisfies neither condition, so neither needs a branch of its own.
    if (magnitude >= java.lang.Double.MIN_NORMAL && magnitude <= Double.MaxValue)
      ().asRight
    // Zero itself is fine; a non-zero number that converts to zero has underflowed.
    // `toBigDecimal` is None only when the exponent overflows an Int, which cannot be zero.
    else if (magnitude === 0.0 && number.toBigDecimal.exists(_.signum === 0))
      ().asRight
    else
      CirceToJsonError.NumberOutOfRange(number.toString).asLeft
  }

  def objectNodeSetAll(node: ObjectNode, fields: java.util.Map[String, JsonNode]): JsonNode =
    node.setAll[JsonNode](fields)

  private def getJsonNodeFromStringContent(content: String): JsonNode =
    mapper.readTree(content)

  @deprecated("Use `circeToJackson(json, maxJsonDepth)`", "3.2.0")
  final def circeToJackson(json: Json): JsonNode =
    circeToJackson(json, Int.MaxValue).toOption.get
}
