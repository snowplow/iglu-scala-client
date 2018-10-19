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
package validator

import cats.data.NonEmptyList

sealed trait ValidatorError {
  def toClientError: ClientError = ClientError.ValidationError(this)
}

object ValidatorError {

  final case class SchemaIssue(path: String, message: String)

  /** Primary error */
  final case class InvalidData(messages: NonEmptyList[ValidatorReport]) extends ValidatorError

  /** Unlike ResolverError.InvalidSchema, this one is more strict */
  final case class InvalidSchema(issues: NonEmptyList[SchemaIssue]) extends ValidatorError

  private[client] def schemaIssue(issue: Throwable): ValidatorError =
    InvalidSchema(NonEmptyList.of(SchemaIssue("$", issue.getMessage)))
}
