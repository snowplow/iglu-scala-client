/*
 * Copyright (c) 2012-2016 Snowplow Analytics Ltd. All rights reserved.
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

// Scala
import scala.collection.JavaConverters.asScalaIteratorConverter

// jackson
import com.fasterxml.jackson.databind.JsonNode

// JsonSchema
import com.github.fge.jsonschema.cfg.ValidationConfiguration
import com.github.fge.jsonschema.core.report.{LogLevel, ProcessingMessage}
import com.github.fge.jsonschema.processors.syntax.SyntaxValidator

object SchemaValidation {
  private val validator: SyntaxValidator =
    new SyntaxValidator(ValidationConfiguration.byDefault())

  /**
   * Get validation errors for Schema
   * Errors like empty `required` property or `minimum` property containing string
   * will be catched
   *
   * @param schema JSON Schema
   * @return list of Processing Messages with log level above warning
   */
  def getErrors(schema: JsonNode): List[ProcessingMessage] =
    validator.validateSchema(schema).iterator.asScala
      .filter(r => (r.getLogLevel == LogLevel.ERROR) || (r.getLogLevel == LogLevel.FATAL))
      .toList

  /**
   * Validate JSON Schema against it's own Schema
   * Errors like empty `required` property or `minimum` property containing string
   * will be catched
   *
   * @param schema JSON Schema
   * @return true if Schema is valid
   */
  def isValid(schema: JsonNode): Boolean =
    getErrors(schema).isEmpty

}

