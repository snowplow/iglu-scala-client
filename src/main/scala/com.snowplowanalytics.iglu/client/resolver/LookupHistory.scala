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
package com.snowplowanalytics.iglu.client.resolver

import cats.Semigroup
import cats.instances.set._
import cats.syntax.semigroup._
import cats.syntax.either._

import io.circe.{Decoder, Encoder, Json}
import io.circe.syntax._

import registries.RegistryError

/**
 * Helper class responsible for aggregating repository lookup errors
 * Using to aggregate all errors for single schema for single repo during all retries
 *
 * @param errors set of all errors happened during all attempts
 * @param attempts amount of undertaken attempts
 * @param fatal indicates whether among failures were unrecoverable ones (like invalid schema)
 */
case class LookupHistory(errors: Set[RegistryError], attempts: Int, fatal: Boolean)

object LookupHistory {

  val MaxErrors = 32

  /** Semigroup instance helping to aggregate repository errors */
  implicit val lookupHistorySemigroup: Semigroup[LookupHistory] =
    new Semigroup[LookupHistory] {
      override def combine(a: LookupHistory, b: LookupHistory): LookupHistory =
        LookupHistory(
          (a.errors |+| b.errors).take(MaxErrors),
          a.attempts.max(b.attempts) + 1,
          a.fatal || b.fatal)
    }

  implicit val lookupHistoryEncoder: Encoder[LookupHistory] = Encoder.instance { history =>
    Json.obj(
      "errors" := history.errors.asJson,
      "attempts" := history.attempts.asJson,
      "fatal" := history.fatal.asJson
    )
  }

  implicit val lookupHistoryDecoder: Decoder[LookupHistory] = Decoder.instance { cursor =>
    for {
      errors   <- cursor.downField("errors").as[Set[RegistryError]]
      attempts <- cursor.downField("attempts").as[Int]
      fatal    <- cursor.downField("fatal").as[Boolean]
    } yield LookupHistory(errors, attempts, fatal)
  }
}
