/*
 * Copyright (c) 2012-2021 Snowplow Analytics Ltd. All rights reserved.
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

// cats
import cats.Functor
import cats.data.NonEmptyList
import cats.implicits._

// circe
import io.circe.Json

import com.snowplowanalytics.iglu.client.resolver.Resolver.ResolverResult

/**
 * Validates that Json conforms to a schema
 *
 * Unlike a simple [[Validator]], the results returned by a ValidatorF are wrapped in an effect type `F`.
 * Implementations of ValidatorF are expected to use caches for more efficient validation.
 */
trait ValidatorF[F[_], A] {

  /**
   * Main method, validating _non-self-describing_ instance
   * @param key an identifier associated with the schema.  Used as a hint for when the validator
   * may use its cache, instead of performing an expensive interpretation of the schema.
   * @param data the value to be checked against the schema
   * @param schema the jsonschema
   */
  def validateF(data: A, schema: ResolverResult[Json]): F[Either[ValidatorError, Unit]]

  /**
   * Get validation errors for a Json Schema
   *
   * Catches errors like empty `required` property or `minimum` property containing string
   *
   * @param key an identifier associated with the schema.  Used as a hint for when the validator
   * may use its cache, instead of performing an expensive interpretation of the schema.
   * @param schema JSON Schema
   * @return list of Processing Messages with log level above warning
   */
  def checkSchemaF(schema: ResolverResult[Json]): F[List[ValidatorError.SchemaIssue]]

  /**
   * Validate a Json Schema
   *
   * @param key an identifier associated with the schema.  Used as a hint for when the validator
   * may use its cache, instead of performing an expensive interpretation of the schema.
   * @param schema JSON Schema
   * @return Either a list of processing Messages with log level above warning (as left) or Unit (as right)
   */
  def validateSchemaF(schema: ResolverResult[Json])(implicit
    F: Functor[F]
  ): F[Either[ValidatorError, Unit]] =
    checkSchemaF(schema).map {
      case Nil    => ().asRight
      case h :: t => ValidatorError.InvalidSchema(NonEmptyList(h, t)).asLeft
    }
}
