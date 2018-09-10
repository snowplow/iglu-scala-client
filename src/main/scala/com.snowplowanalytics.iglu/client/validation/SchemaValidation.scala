/*
 * Copyright (c) 2012-2018 Snowplow Analytics Ltd. All rights reserved.
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
import scala.collection.JavaConverters._

// Jackson
import com.fasterxml.jackson.databind.ObjectMapper

// circe
import io.circe.Json

// JsonSchema
import com.networknt.schema._

object SchemaValidation {

  private lazy val v4MetaSchema =
    new ObjectMapper().readTree(getClass.getResourceAsStream("/v4metaschema.json"))

  private lazy val v4Schema = JsonSchemaFactory.getInstance.getSchema(v4MetaSchema)

  /**
   * Get validation errors for Schema
   * Errors like empty `required` property or `minimum` property containing string
   * will be catched
   *
   * @param schema JSON Schema
   * @return list of Processing Messages with log level above warning
   */
  def getErrors(schema: Json): List[ProcessingMessage] = {
    val jacksonJson = new ObjectMapper().readTree(schema.noSpaces)
    v4Schema
      .validate(jacksonJson)
      .asScala
      .toList
      .map(m => ProcessingMessage(m.getMessage, jsonPath = Some(m.getPath)))
  }

  /**
   * Validate JSON Schema against it's own Schema
   * Errors like empty `required` property or `minimum` property containing string
   * will be catched
   *
   * @param schema JSON Schema
   * @return true if Schema is valid
   */
  def isValid(schema: Json): Boolean = getErrors(schema).isEmpty
}
