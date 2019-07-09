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

import java.time.Instant

import cats.Show
import cats.syntax.show._
import cats.syntax.either._

import io.circe.{Decoder, DecodingFailure, Encoder, Json}
import io.circe.syntax._

import validator.ValidatorError
import resolver.LookupHistory
import resolver.registries.RegistryError

/** Common type for Resolver's and Validator's errors */
sealed trait ClientError extends Product with Serializable {
  def getMessage: String =
    ClientError.igluClientResolutionErrorCirceEncoder(this).noSpaces
}

object ClientError {

  /** Error happened during schema resolution step */
  final case class ResolutionError(value: Map[String, LookupHistory]) extends ClientError {
    def isNotFound: Boolean =
      value.values.flatMap(_.errors).forall(_ == RegistryError.NotFound)
  }

  /** Error happened during schema/instance validation step */
  final case class ValidationError(error: ValidatorError) extends ClientError

  implicit val igluClientResolutionErrorCirceEncoder: Encoder[ClientError] =
    Encoder.instance {
      case ResolutionError(lookupHistory) =>
        Json.obj(
          "error" := Json.fromString("ResolutionError"),
          "lookupHistory" := lookupHistory.toList
            .map {
              case (repo, lookups) =>
                lookups.asJson.deepMerge(Json.obj("repository" := repo.asJson))
            }
        )
      case ValidationError(error) =>
        error.asJson.deepMerge(Json.obj("error" := Json.fromString("ValidationError")))
    }

  implicit val igluClientResolutionErrorCirceDecoder: Decoder[ClientError] =
    Decoder.instance { cursor =>
      for {
        error <- cursor.downField("error").as[String]
        result <- error match {
          case "ResolutionError" =>
            cursor
              .downField("lookupHistory")
              .as[List[RepoLookupHistory]]
              .map { history =>
                ResolutionError(history.map(_.toField).toMap)
              }
          case "ValidationError" =>
            cursor
              .as[ValidatorError]
              .map { error =>
                ValidationError(error)
              }
          case _ =>
            DecodingFailure(
              s"Error type $error cannot be recognized as Iglu Client Error",
              cursor.history).asLeft
        }

      } yield result

    }

  implicit val igluClientShowInstance: Show[ClientError] =
    Show.show {
      case ClientError.ValidationError(ValidatorError.InvalidData(reports)) =>
        val issues = reports.toList
          .groupBy(_.path)
          .map {
            case (path, messages) =>
              s"* At ${path.getOrElse("unknown path")}:\n" ++ messages
                .map(_.message)
                .map(m => s"  - $m")
                .mkString("\n")
          }
        s"Instance is not valid against its schema:\n${issues.mkString("\n")}"
      case ClientError.ValidationError(ValidatorError.InvalidSchema(reports)) =>
        val r = reports.toList.map(i => s"* [${i.message}] (at ${i.path})").mkString(",\n")
        s"Resolved schema cannot be used to validate an instance. Following issues found:\n$r"
      case ClientError.ResolutionError(lookup) =>
        val attempts = (a: Int) => if (a == 1) "1 attempt" else s"$a attempts"
        val errors = lookup.map {
          case (repo, tries) =>
            s"* $repo due [${tries.errors.map(_.show).mkString(", ")}] after ${attempts(tries.attempts)}"
        }
        s"Schema cannot be resolved in following repositories:\n${errors.mkString("\n")}"
    }

  // Auxiliary entity, helping to decode Map[String, LookupHistory]
  private case class RepoLookupHistory(
    repository: String,
    errors: Set[RegistryError],
    attempts: Int,
    lastAttempt: Instant) {
    def toField: (String, LookupHistory) =
      (repository, LookupHistory(errors, attempts, lastAttempt))
  }

  private object RepoLookupHistory {
    implicit val repoLookupHistoryDecoder: Decoder[RepoLookupHistory] =
      Decoder.instance { cursor =>
        for {
          repository <- cursor.downField("repository").as[String]
          errors     <- cursor.downField("errors").as[Set[RegistryError]]
          attempts   <- cursor.downField("attempts").as[Int]
          last       <- cursor.downField("lastAttempt").as[Instant]
        } yield RepoLookupHistory(repository, errors, attempts, last)
      }
  }
}
