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
import cats.data.NonEmptyList
import cats.syntax.either._

// circe
import io.circe.Json

trait Validator[A] {

  /** Main method, validating _non-self-describing_ instance */
  def validate(data: A, schema: Json): Either[ValidatorError, Unit]

  /**
   * Get validation errors for Schema
   * Errors like empty `required` property or `minimum` property containing string
   * will be catched
   *
   * @param schema JSON Schema
   * @return list of Processing Messages with log level above warning
   */
  def checkSchema(schema: Json): List[ValidatorError.SchemaIssue]

  def validateSchema(schema: Json): Either[ValidatorError, Unit] =
    checkSchema(schema) match {
      case Nil    => ().asRight
      case h :: t => ValidatorError.InvalidSchema(NonEmptyList(h, t)).asLeft
    }

  /**
   * Validate JSON Schema against it's own Schema
   * Errors like empty `required` property or `minimum` property containing string
   * will be catched
   */
  def isValidSchema(schema: Json): Boolean = checkSchema(schema).isEmpty
}
