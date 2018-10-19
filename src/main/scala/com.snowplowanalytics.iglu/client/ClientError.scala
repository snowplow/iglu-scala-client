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

// cats
import cats.data.NonEmptyList

import validator.ValidatorReport
import resolver.LookupHistory
import resolver.registries.RegistryError

sealed trait ClientError

object ClientError {

  /** Error happened during schema resolution step */
  case class ResolverError(value: Map[String, LookupHistory]) extends ClientError {
    def isNotFound: Boolean =
      value.values.flatMap(_.errors).forall(_ == RegistryError.NotFound)
  }

  /** Error happened during schema/instance validation step */
  case class ValidationError(error: ValidatorError) extends ClientError

  sealed trait ValidatorError {
    def toClientError: ClientError = ValidationError(this)
  }

  object ValidatorError {

    case class SchemaIssue(path: String, message: String)

    /** Primary error */
    case class InvalidData(messages: NonEmptyList[ValidatorReport]) extends ValidatorError

    /** Unlike ResolverError.InvalidSchema, this one is more strict */
    case class InvalidSchema(issues: NonEmptyList[SchemaIssue]) extends ValidatorError

    private[client] def schemaIssue(issue: Throwable): ValidatorError =
      InvalidSchema(NonEmptyList.of(SchemaIssue("$", issue.getMessage)))
  }
}
