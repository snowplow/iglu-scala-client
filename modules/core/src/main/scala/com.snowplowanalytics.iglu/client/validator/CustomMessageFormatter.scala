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
package com.snowplowanalytics.iglu.client
package validator

import java.text.MessageFormat
import java.util.Properties
import scala.jdk.CollectionConverters._
import com.fasterxml.jackson.databind.ObjectMapper
import com.fasterxml.jackson.databind.JsonNode
import scala.util.Try

/**
 * Custom message formatter that maintains backward-compatible error messages
 * regardless of json-schema-validator library version changes.
 *
 * Message templates are frozen at version 1.0.76 format to prevent breaking
 * changes for downstream consumers (e.g., Enrich, DQD) that parse error messages.
 *
 * Templates are loaded from jsv-messages-1.0.76.properties, which is a copy of
 * the original file from json-schema-validator 1.0.76.
 */
object CustomMessageFormatter {

  /**
   * ObjectMapper for parsing JSON arrays in validator arguments.
   * Used to robustly handle enum values that may contain special characters or quotes.
   * Lazy to avoid initialization cost unless enum validation is used.
   * Thread-safe and reused across all validations for efficiency.
   */
  private lazy val objectMapper = new ObjectMapper()

  /**
   * Message templates from json-schema-validator 1.0.76.
   * Format uses Java MessageFormat placeholders: {0}, {1}, {2}, etc.
   *
   * Template arguments:
   * - {0} is always the JSON path (e.g., "$.field" or "$")
   * - {1}, {2}, ... are validator-specific arguments
   *
   * Loaded from jsv-messages-1.0.76.properties (43 templates total)
   * Pre-compiled as MessageFormat objects for better performance.
   * Lazy to avoid initialization cost when validation is not used.
   */
  private lazy val messageTemplates: Map[String, MessageFormat] = {
    val props  = new Properties()
    val stream = getClass.getClassLoader.getResourceAsStream("jsv-messages-1.0.76.properties")
    if (stream == null) {
      throw new IllegalStateException("Could not find jsv-messages-1.0.76.properties on classpath")
    }
    try {
      props.load(stream)
      props.asScala.map { case (key, template) =>
        key -> new MessageFormat(template)
      }.toMap
    } finally
      stream.close()
  }

  /**
   * Transform validator arguments from 1.5.8 library format to 1.0.76 format.
   *
   * Handles validators where only arguments changed (enum, format, not, oneOf).
   * For validators with completely different messages (additionalItems), see formatMessage().
   *
   * @param validatorType The validator keyword (e.g., "enum", "format")
   * @param arguments Raw arguments from the validator library
   * @param schemaNode Optional schema node, needed for oneOf to extract schema details
   * @return Transformed arguments matching 1.0.76 format
   */
  def transformArguments(
    validatorType: String,
    arguments: Seq[String],
    schemaNode: Option[JsonNode] = None
  ): Seq[String] = {
    validatorType match {
      // enum: 1.5.8 quotes the values like ["foo", "bar"] but 1.0.76 produced [foo, bar]
      // See https://github.com/networknt/json-schema-validator/pull/1095
      case "enum" if arguments.nonEmpty =>
        arguments.map { arg =>
          // Use Jackson to parse the JSON array properly and rebuild without quotes
          Try {
            val arrayNode = objectMapper.readTree(arg)
            if (arrayNode.isArray) {
              val elements = arrayNode.elements().asScala.toList.map { node =>
                // Get the value without JSON encoding (no quotes for strings)
                if (node.isTextual) node.asText()
                else node.toString
              }
              s"[${elements.mkString(", ")}]"
            } else {
              arg
            }
          }.getOrElse(arg) // Fallback to original if parsing fails
        }

      // format: 1.5.8 passes [formatName, regexPattern, actualValue] (3 args)
      // but 1.0.76 passed [formatName, regexPattern] (2 args)
      // Drop the third argument (actualValue) to match 1.0.76
      case "format" if arguments.size == 3 =>
        arguments.take(2)

      // not: 1.5.8 includes schema location in argument like "$.not" : {"type":"string"}
      // but 1.0.76 just used "not" : {"type":"string"}
      // Strip the schema location prefix from the argument
      case "not" if arguments.nonEmpty =>
        arguments.map { arg =>
          // Transform "$.not" : {...} to "not" : {...}
          arg.replaceFirst("\"\\$\\.not\"", "\"not\"")
        }

      // oneOf: 1.5.8 passes ["2", "0, 1"] (count and matching indices)
      // but 1.0.76 passed ["{\"type\":\"string\"}{\"type\":\"number\"}"] (concatenated schemas)
      // We extract schemas at indices 0 and 1 from schemaNode and concatenate their JSON
      case "oneOf" if arguments.size >= 2 && schemaNode.exists(_.isArray) =>
        val indicesStr = arguments(1)
        val indices = indicesStr
          .split(",\\s*")
          .flatMap { s =>
            Try(s.trim.toInt).toOption
          }
          .toList
        val schemas = schemaNode.toList.flatMap { node =>
          indices.flatMap { idx =>
            Option(node.get(idx)).map(_.toString)
          }
        }
        if (schemas.nonEmpty) List(schemas.mkString) else arguments

      case _ => arguments
    }
  }

  /**
   * Format a validation error message using our frozen templates.
   *
   * @param validatorType The validator keyword (e.g., "required", "maxLength")
   * @param path The JSON path where validation failed (e.g., "$.field")
   * @param arguments Validator-specific arguments already transformed to 1.0.76 format
   * @return Formatted error message matching 1.0.76 format, or None if template not found
   */
  def formatMessage(
    validatorType: String,
    path: String,
    arguments: Seq[String]
  ): Option[String] = {
    // Special handling for validators with completely different message formats (additionalItems).
    // For validators where only arguments changed (enum, format, not, oneOf), see transformArguments().
    validatorType match {
      // additionalItems: In 1.5.8, produces "$: index '2' is not defined in the schema and the schema does not allow additional items"
      // but 1.0.76 produced "$[2]: no validator found at this index"
      // Message format is completely different, so we reconstruct it manually
      case "additionalItems" if arguments.nonEmpty =>
        val index = arguments.head
        Some(s"$path[$index]: no validator found at this index")

      case _ =>
        messageTemplates.get(validatorType).flatMap { formatter =>
          // Build arguments array: [path, arg1, arg2, ...]
          // This matches the validator library's ValidationMessage.Builder.build() logic
          // Note: arguments should already be transformed to 1.0.76 format by the caller
          val args = (path +: arguments).toArray[AnyRef]
          // Use Try to protect against future library changes
          // If formatting fails, return None to fall back to library's default message
          Try(formatter.format(args)).toOption
        }
    }
  }
}
