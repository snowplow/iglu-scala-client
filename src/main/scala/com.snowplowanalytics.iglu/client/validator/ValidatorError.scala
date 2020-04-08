/*
 * Copyright (c) 2014-2019 Snowplow Analytics Ltd. All rights reserved.
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

import io.circe._
import io.circe.syntax._

import cats.data.NonEmptyList

/** ADT describing issues that can be discovered by Validator */
sealed trait ValidatorError extends Product with Serializable {
  def toClientError: ClientError = ClientError.ValidationError(this)
}

object ValidatorError {

  /** Primary error, indicating there are invalid properties in JSON instance */
  final case class InvalidData(messages: NonEmptyList[ValidatorReport]) extends ValidatorError

  /** Unlike ResolverError.InvalidSchema, this one is more strict */
  final case class InvalidSchema(issues: NonEmptyList[SchemaIssue]) extends ValidatorError

  /**
   * Short human-readable description of problem discovered in JSON Schema
   * @param path JSON Path to problematic schema property
   * @param message human-readable error message
   */
  final case class SchemaIssue(path: String, message: String)

  private[client] def schemaIssue(issue: Throwable): ValidatorError =
    InvalidSchema(NonEmptyList.of(SchemaIssue("$", issue.getMessage)))

  implicit val validatorErrorJsonEncoder: Encoder.AsObject[ValidatorError] =
    Encoder.AsObject.instance {
      case InvalidData(messages) => JsonObject.fromMap(Map("dataReports"  -> messages.asJson))
      case InvalidSchema(issues) => JsonObject.fromMap(Map("schemaIssues" -> issues.asJson))
    }

  implicit val validatorErrorJsonDecoder: Decoder[ValidatorError] =
    Decoder.instance { cursor =>
      for {
        validatorError <- cursor.downField("dataReports").focus match {
          case Some(dataReports) =>
            dataReports.as[NonEmptyList[ValidatorReport]].map(reps => InvalidData(reps))
          case None =>
            cursor
              .downField("schemaIssues")
              .as[NonEmptyList[SchemaIssue]]
              .map(reps => InvalidSchema(reps))
        }
      } yield validatorError
    }

  implicit val schemaIssueCirceJsonEncoder: Encoder[SchemaIssue] =
    Encoder.instance { issue =>
      Json.obj("path" := issue.path, "message" := issue.message)
    }

  implicit val schemaIssueCirceJsonDecoder: Decoder[SchemaIssue] =
    Decoder.instance { cursor =>
      for {
        path    <- cursor.downField("path").as[String]
        message <- cursor.downField("message").as[String]
      } yield SchemaIssue(path, message)
    }
}
