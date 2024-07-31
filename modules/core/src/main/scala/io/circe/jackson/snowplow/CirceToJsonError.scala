/*
 * Copyright (c) 2014-2024 Snowplow Analytics Ltd. All rights reserved.
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
package io.circe.jackson.snowplow

import cats.data.NonEmptyList
import com.snowplowanalytics.iglu.client.validator.{ValidatorError, ValidatorReport}

sealed trait CirceToJsonError extends Product with Serializable {
  def message: String

  def toInvalidData: ValidatorError.InvalidData =
    ValidatorError.InvalidData(
      NonEmptyList.one(
        ValidatorReport(message, Some("/"), List.empty, None)
      )
    )

  def toInvalidSchema: ValidatorError.InvalidSchema =
    ValidatorError.InvalidSchema(NonEmptyList.one(toSchemaIssue))

  def toSchemaIssue: ValidatorError.SchemaIssue =
    ValidatorError.SchemaIssue(path = "/", message = message)
}

object CirceToJsonError {
  case object MaxDepthExceeded extends CirceToJsonError {
    override def message: String = "Maximum allowed JSON depth exceeded"
  }
}
