/*
 * Copyright (c) 2014-2018 Snowplow Analytics Ltd. All rights reserved.
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

// Cats
import cats.syntax.either._
import cats.syntax.functor._
import cats.data.NonEmptyList
import cats.effect.Sync

// Iglu Core
import com.snowplowanalytics.iglu.core.{SchemaCriterion, SchemaKey}

/**
 * Interface common for JSON AST that can be validated with Resolver
 *
 * @tparam JsonAST AST to validate (eg circe's Json)
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
   *         Json, or a Failure boxing
   *         a NonEmptyList of
   *         ProcessingMessages
   */
  def validateAgainstSchema(
    instance: JsonAST,
    schema: JsonAST): Either[NonEmptyList[ProcessingMessage], JsonAST]

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
   * @param dataOnly Whether the returned Json
   *        should be the data only, or the whole
   *        JSON (schema + data)
   * @return either Success boxing the Json
   *         or a Failure boxing a NonEmptyList
   *         of ProcessingMessages
   */
  def validate[F[_]: Sync](
    resolver: Resolver[F],
    instance: JsonAST,
    dataOnly: Boolean = false): F[Either[NonEmptyList[ProcessingMessage], JsonAST]] =
    validateAndIdentifySchema(resolver, instance, dataOnly).map(_.map(_._2))

  /**
   * The same as validate(), but on Success returns
   * a tuple containing the SchemaKey as well as
   * the Json.
   *
   * IMPORTANT: currently the exact version of
   * the JSON Schema (i.e. MODEL-REVISION-ADDITION)
   * must be resolvable thru Iglu.
   *
   * @param instance The self-describing JSON
   *         to validate
   * @param dataOnly Whether the returned Json
   *        should be the data only, or the whole
   *        JSON (schema + data)
   * @return either Success boxing a Tuple2 of the
   *         JSON's SchemaKey plus its Json,
   *         or a Failure boxing a NonEmptyList
   *         of ProcessingMessages
   */
  def validateAndIdentifySchema[F[_]: Sync](
    resolver: Resolver[F],
    instance: JsonAST,
    dataOnly: Boolean = false): F[Either[NonEmptyList[ProcessingMessage], (SchemaKey, JsonAST)]]

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
   * @param dataOnly Whether the returned Json
   *        should be the data only, or the whole
   *        JSON (schema + data)
   * @return either Success boxing the Json
   *         or a Failure boxing a NonEmptyList
   *         of ProcessingMessages
   */
  def verifySchemaAndValidate[F[_]: Sync](
    resolver: Resolver[F],
    instance: JsonAST,
    schemaCriterion: SchemaCriterion,
    dataOnly: Boolean = false): F[Either[NonEmptyList[ProcessingMessage], JsonAST]]

  /**
   * Operations available as postfix-ops
   * WARNING: this cannot be used for two ASTs with one import
   *
   * @param instance JSON instance, supposed to be self-describing
   */
  implicit class ValidatableOps(val instance: JsonAST) {

    /**
     * Validates a JSON against a given JSON Schema. On Success, simply
     * passes through the original JSON.
     * On Failure, return a NonEmptyList of failure messages.
     *
     * @param schema The JSON Schema to validate the JSON against
     * @return either Success boxing the Json, or a Failure boxing
     *         a NonEmptyList of ProcessingMessages
     */
    def validateAgainstSchema(schema: JsonAST): Either[NonEmptyList[ProcessingMessage], JsonAST] =
      self.validateAgainstSchema(instance, schema)

    /**
     * Validates a self-describing JSON against its specified JSON Schema.
     *
     * @param dataOnly Whether the returned Json should be the data only, or the whole
     *        JSON (schema + data)
     * @return either Success boxing the Json or a Failure boxing a NonEmptyList
     *         of ProcessingMessages
     */
    def validate[F[_]: Sync](
      resolver: Resolver[F],
      dataOnly: Boolean): F[Either[NonEmptyList[ProcessingMessage], JsonAST]] =
      self.validate(resolver, instance, dataOnly)

    /**
     * The same as validate(), but on Success returns a tuple containing the SchemaKey as well as
     * the Json.
     *
     * @param dataOnly Whether the returned Json should be the data only, or the whole
     *        JSON (schema + data)
     * @return either Success boxing a Tuple2 of the JSON's SchemaKey plus its Json,
     *         or a Failure boxing a NonEmptyList of ProcessingMessages
     */
    def validateAndIdentifySchema[F[_]: Sync](
      resolver: Resolver[F],
      dataOnly: Boolean): F[Either[NonEmptyList[ProcessingMessage], (SchemaKey, JsonAST)]] =
      self.validateAndIdentifySchema(resolver, instance, dataOnly)

    /**
     * Verify that this JSON is of the expected schema, then validate it against the schema.
     *
     * @param schemaCriterion Identifying the schema we believe this JSON is described by
     * @param dataOnly Whether the returned Json should be the data only, or the whole
     *        JSON (schema + data)
     * @return either Success boxing the Json or a Failure boxing a NonEmptyList
     *         of ProcessingMessages
     */
    def verifySchemaAndValidate[F[_]: Sync](
      resolver: Resolver[F],
      schemaCriterion: SchemaCriterion,
      dataOnly: Boolean): F[Either[NonEmptyList[ProcessingMessage], JsonAST]] =
      self.verifySchemaAndValidate(resolver, instance, schemaCriterion, dataOnly)
  }
}
