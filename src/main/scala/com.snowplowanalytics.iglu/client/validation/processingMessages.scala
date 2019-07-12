/*
 * Copyright (c) 2014 Snowplow Analytics Ltd. All rights reserved.
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

// Cats
import cats.data.{NonEmptyList, Validated, ValidatedNel}

// Circe
import io.circe._
import io.circe.syntax._

// TODO: replace with better format after discussion
case class ProcessingMessage(
  message: String,
  schemaKey: Option[String] = None,
  repositories: Option[Json] = None) {

  def asJson: Json = {
    Json.obj(
      "level" := "error",
      "message" := message,
      "schemaKey" := schemaKey
    )
  }

}

/**
 * Makes it easier to work with ProcessingMessages.
 */
object ProcessingMessageMethods {

  /**
   * A helper method to convert a String into
   * a ProcessingMessage. Assume that the
   * String is coming from a Validation's
   * Failure, so the LogLevel is ERROR.
   *
   * @param message The error message or other
   *        String for our ProcessingMessage
   * @param logLevel The log level for this
   *        ProcessingMessage. Defaults to
   *        ERROR
   * @return the generated ProcessingMessage
   *         with message and log level set
   */
  def toProcMsg(message: String): ProcessingMessage =
    ProcessingMessage(message)

  def toProcMsg(messages: NonEmptyList[String]): ProcessingMessageNel =
    messages.map(msg => toProcMsg(msg))

  def toProcMsgNel(message: String): ProcessingMessageNel =
    NonEmptyList.one(toProcMsg(message))

  /**
   * A wrapper for the Cats Validated, to make it easy to convert
   * Strings to ProcessingMessages on the Failure side of
   * Validations.
   */
  implicit class ProcMsgValidation[+A](val validation: Validated[String, A]) extends AnyVal {

    def toProcessingMessage: Validated[ProcessingMessage, A] =
      validation.leftMap { err =>
        ProcessingMessageMethods.toProcMsg(err)
      }

    def toProcessingMessageNel: ValidatedNel[ProcessingMessage, A] =
      toProcessingMessage.toValidatedNel
  }

  /**
   * A wrapper for the Scalaz Validation, to make it easy to convert
   * Strings to ProcessingMessages on the Failure side of
   * Validations.
   */
  implicit class ProcMsgValidationNel[+A](val validation: ValidatedNel[String, A]) extends AnyVal {

    def toProcessingMessages: ValidatedNel[ProcessingMessage, A] =
      validation.leftMap { err =>
        ProcessingMessageMethods.toProcMsg(err)
      }
  }

  /**
   * A wrapper to make it easy to convert a String to a ProcessingMessage.
   */
  implicit class ProcMsgString(val str: String) extends AnyVal {

    def toProcessingMessage: ProcessingMessage =
      ProcessingMessageMethods.toProcMsg(str)

    def toProcessingMessageNel: ProcessingMessageNel =
      ProcessingMessageMethods.toProcMsgNel(str)
  }

}
