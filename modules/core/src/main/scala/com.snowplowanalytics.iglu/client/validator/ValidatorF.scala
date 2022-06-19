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

trait ValidatorF[F[_], K, A] {

  /** Main method, validating _non-self-describing_ instance */
  def validateF(key: Option[K], data: A, schema: Json): F[Either[ValidatorError, Unit]]

  /**
   * Get validation errors for Schema
   * Errors like empty `required` property or `minimum` property containing string
   * will be catched
   *
   * @param schema JSON Schema
   * @return list of Processing Messages with log level above warning
   */
  def checkSchemaF(key: Option[K], schema: Json): F[List[ValidatorError.SchemaIssue]]

  def validateSchemaF(key: Option[K], schema: Json)(implicit
    F: Functor[F]
  ): F[Either[ValidatorError, Unit]] =
    checkSchemaF(key, schema).map {
      case Nil    => ().asRight
      case h :: t => ValidatorError.InvalidSchema(NonEmptyList(h, t)).asLeft
    }

  /**
   * Validate JSON Schema against it's own Schema
   * Errors like empty `required` property or `minimum` property containing string
   * will be catched
   */
  def isValidSchemaF(key: Option[K], schema: Json)(implicit F: Functor[F]): F[Boolean] =
    checkSchemaF(key, schema).map(_.isEmpty)
}
