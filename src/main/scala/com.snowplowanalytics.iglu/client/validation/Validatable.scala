/*
 * Copyright (c) 2014-2017 Snowplow Analytics Ltd. All rights reserved.
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
import scala.language.implicitConversions

/**
 * Interface common for JSON AST that can be validated with Resolver
 *
 * @tparam JsonAST AST to validate (jackson `JsonNode` or `JValue`)
 */
trait Validatable[JsonAST] { self =>

  /**
   * Validates a JSON against a given
   * JSON Schema. On Success, simply
   * passes through the original JSON.
   * On Failure, return a NonEmptyList
   * of failure messages.
   *
   * @param instance The JSON to validate
   * @param schema The JSON Schema to
   *        validate the JSON against
   * @return either Success boxing the
   *         JsonNode, or a Failure boxing
   *         a NonEmptyList of
   *         ProcessingMessages
   */
  def validateAgainstSchema(instance: JsonAST, schema: JsonAST): ValidatedNelType[JsonAST]

  /**
   * Validates a self-describing JSON against
   * its specified JSON Schema.
   *
   * IMPORTANT: currently the exact version of
   * the JSON Schema (i.e. MODEL-REVISION-ADDITION)
   * must be resolvable thru Iglu.
   *
   * @param instance The self-describing JSON
   *         to validate
   * @param dataOnly Whether the returned JsonNode
   *        should be the data only, or the whole
   *        JSON (schema + data)
   * @return either Success boxing the JsonNode
   *         or a Failure boxing a NonEmptyList
   *         of ProcessingMessages
   */
  def validate(instance: JsonAST, dataOnly: Boolean = false)(
    implicit resolver: Resolver): ValidatedNelType[JsonAST]

  /**
   * The same as validate(), but on Success returns
   * a tuple containing the SchemaKey as well as
   * the JsonNode.
   *
   * IMPORTANT: currently the exact version of
   * the JSON Schema (i.e. MODEL-REVISION-ADDITION)
   * must be resolvable thru Iglu.
   *
   * @param instance The self-describing JSON
   *         to validate
   * @param dataOnly Whether the returned JsonNode
   *        should be the data only, or the whole
   *        JSON (schema + data)
   * @return either Success boxing a Tuple2 of the
   *         JSON's SchemaKey plus its JsonNode,
   *         or a Failure boxing a NonEmptyList
   *         of ProcessingMessages
   */
  def validateAndIdentifySchema(instance: JsonAST, dataOnly: Boolean = false)(
    implicit resolver: Resolver): ValidatedNelType[(SchemaKey, JsonAST)]

  /**
   * Verify that this JSON is of the expected schema,
   * then validate it against the schema.
   *
   * IMPORTANT: currently the exact version of
   * the JSON Schema (i.e. MODEL-REVISION-ADDITION)
   * must be resolvable thru Iglu.
   *
   * @param instance The self-describing JSON to
   *        verify and validate
   * @param schemaCriterion Identifying the schema we
   *        believe this JSON is described by
   * @param dataOnly Whether the returned JsonNode
   *        should be the data only, or the whole
   *        JSON (schema + data)
   * @return either Success boxing the JsonNode
   *         or a Failure boxing a NonEmptyList
   *         of ProcessingMessages
   */
  def verifySchemaAndValidate(
    instance: JsonAST,
    schemaCriterion: SchemaCriterion,
    dataOnly: Boolean = false)(implicit resolver: Resolver): ValidatedNelType[JsonAST]

  /**
   * Operations available as postfix-ops
   * WARNING: this cannot be used for two ASTs with one import
   *
   * @param instance JSON instance, supposed to be self-describing
   */
  implicit class ValidatableOps(instance: JsonAST) {

    /**
     * Validates a JSON against a given JSON Schema. On Success, simply
     * passes through the original JSON.
     * On Failure, return a NonEmptyList of failure messages.
     *
     * @param schema The JSON Schema to validate the JSON against
     * @return either Success boxing the JsonNode, or a Failure boxing
     *         a NonEmptyList of ProcessingMessages
     */
    def validateAgainstSchema(schema: JsonAST)(
      implicit resolver: Resolver): ValidatedNelType[JsonAST] =
      self.validateAgainstSchema(instance, schema)

    /**
     * Validates a self-describing JSON against its specified JSON Schema.
     *
     * @param dataOnly Whether the returned JsonNode should be the data only, or the whole
     *        JSON (schema + data)
     * @return either Success boxing the JsonNode or a Failure boxing a NonEmptyList
     *         of ProcessingMessages
     */
    def validate(dataOnly: Boolean)(implicit resolver: Resolver): ValidatedNelType[JsonAST] =
      self.validate(instance, dataOnly)

    /**
     * The same as validate(), but on Success returns a tuple containing the SchemaKey as well as
     * the JsonNode.
     *
     * @param dataOnly Whether the returned JsonNode should be the data only, or the whole
     *        JSON (schema + data)
     * @return either Success boxing a Tuple2 of the JSON's SchemaKey plus its JsonNode,
     *         or a Failure boxing a NonEmptyList of ProcessingMessages
     */
    def validateAndIdentifySchema(dataOnly: Boolean)(
      implicit resolver: Resolver): ValidatedNelType[(SchemaKey, JsonAST)] =
      self.validateAndIdentifySchema(instance, dataOnly)

    /**
     * Verify that this JSON is of the expected schema, then validate it against the schema.
     *
     * @param schemaCriterion Identifying the schema we believe this JSON is described by
     * @param dataOnly Whether the returned JsonNode should be the data only, or the whole
     *        JSON (schema + data)
     * @return either Success boxing the JsonNode or a Failure boxing a NonEmptyList
     *         of ProcessingMessages
     */
    def verifySchemaAndValidate(schemaCriterion: SchemaCriterion, dataOnly: Boolean)(
      implicit resolver: Resolver): ValidatedNelType[JsonAST] =
      self.verifySchemaAndValidate(instance, schemaCriterion, dataOnly)
  }
}
