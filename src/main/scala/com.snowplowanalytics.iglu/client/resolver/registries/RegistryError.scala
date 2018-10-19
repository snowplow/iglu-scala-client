/*
 * Copyright (c) 2018-2019 Snowplow Analytics Ltd. All rights reserved.
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
package com.snowplowanalytics.iglu.client.resolver.registries

import cats.Show
import cats.syntax.either._

import io.circe.{Decoder, DecodingFailure, Encoder, Json}
import io.circe.syntax._

sealed trait RegistryError extends Product with Serializable

object RegistryError {

  /** Schema certainly does not exist on this registry */
  final case object NotFound extends RegistryError

  /** Other error, e.g. 500 HTTP status or invalid schema. Usually, non fatal */
  final case class RepoFailure(message: String) extends RegistryError

  /** Internal error, usually due configuration error. Can be considered fatal */
  final case class ClientFailure(message: String) extends RegistryError

  final implicit val registryErrorCirceJsonEncoder: Encoder[RegistryError] =
    Encoder.instance {
      case NotFound =>
        Json.obj("error" := Json.fromString("NotFound"))
      case RepoFailure(message) =>
        Json.obj(
          "error" := Json.fromString("RepoFailure"),
          "message" := Json.fromString(message)
        )
      case ClientFailure(message) =>
        Json.obj(
          "error" := Json.fromString("ClientFailure"),
          "message" := Json.fromString(message)
        )
    }

  final implicit val registryErrorCirceJsonDecoder: Decoder[RegistryError] =
    Decoder.instance { cursor =>
      for {
        error <- cursor.downField("error").as[Option[String]]
        message = cursor.downField("message").as[String]
        registryError <- error match {
          case Some("RepoFailure")   => message.map(m => RepoFailure(m))
          case Some("ClientFailure") => message.map(m => ClientFailure(m))
          case Some("NotFound")      => NotFound.asRight
          case _                     => DecodingFailure("RegistryError is not recognized", cursor.history).asLeft
        }
      } yield registryError
    }

  implicit val registryErrorShow: Show[RegistryError] =
    Show.show {
      case NotFound               => "NotFound"
      case RepoFailure(message)   => s"Iglu Repository Failure. $message"
      case ClientFailure(message) => s"Iglu Client Failure. $message"
    }
}
