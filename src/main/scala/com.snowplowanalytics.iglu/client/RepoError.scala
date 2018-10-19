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

import cats.Semigroup
import cats.instances.set._
import cats.syntax.semigroup._

import com.snowplowanalytics.iglu.client.validation.ProcessingMessage

/**
 * Helper class responsible for aggregating repository lookup errors
 * Using to aggregate all errors for single schema for single repo during all retries
 *
 * @param errors set of all errors happened during all attempts
 * @param attempts amount of undertaken attempts
 * @param unrecoverable indicates whether among failures were unrecoverable ones (like invalid schema)
 */
private[client] case class RepoError(
  errors: Set[ProcessingMessage],
  attempts: Int,
  unrecoverable: Boolean)

private[client] object RepoError {

  /**
   * Semigroup instance helping to aggregate repository errors
   */
  implicit object RepoErrorSemigroup extends Semigroup[RepoError] {
    override def combine(a: RepoError, b: RepoError): RepoError =
      RepoError(
        a.errors |+| b.errors,
        a.attempts.max(b.attempts) + 1,
        a.unrecoverable || b.unrecoverable)
  }
}
