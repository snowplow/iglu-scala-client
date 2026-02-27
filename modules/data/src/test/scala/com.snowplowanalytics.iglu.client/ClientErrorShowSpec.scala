/*
 * Copyright (c) 2014-2023 Snowplow Analytics Ltd. All rights reserved.
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

import cats.data.NonEmptyList
import cats.syntax.show._

import com.snowplowanalytics.iglu.client.resolver.LookupHistory
import com.snowplowanalytics.iglu.client.resolver.registries.RegistryError
import com.snowplowanalytics.iglu.client.validator.{ValidatorError, ValidatorReport}

import org.specs2.Specification

import java.time.Instant
import scala.collection.immutable.SortedMap

class ClientErrorShowSpec extends Specification {

  val now: Instant = Instant.ofEpochSecond(1562598285)

  def is = s2"""
  Show instance for ClientError should
    display InvalidData with path             $e1
    display InvalidData with unknown path     $e2
    display InvalidSchema                     $e3
    display ResolutionError with one attempt  $e4
    display ResolutionError with many attempts $e5
  getMessage should return JSON encoding      $e6
  ResolutionError.isNotFound should
    return true when all errors are NotFound     $e7
    return false when errors include RepoFailure $e8
  """

  def e1 = {
    val error: ClientError = ClientError.ValidationError(
      ValidatorError.InvalidData(
        NonEmptyList.of(
          ValidatorReport("field is required", Some("$.name"), Nil, Some("required"))
        )
      ),
      None
    )
    val shown = error.show
    (shown must contain("Instance is not valid against its schema")) and
      (shown must contain("$.name")) and
      (shown must contain("field is required"))
  }

  def e2 = {
    val error: ClientError = ClientError.ValidationError(
      ValidatorError.InvalidData(
        NonEmptyList.of(
          ValidatorReport("something wrong", None, Nil, None)
        )
      ),
      None
    )
    val shown = error.show
    (shown must contain("unknown path")) and
      (shown must contain("something wrong"))
  }

  def e3 = {
    val error: ClientError = ClientError.ValidationError(
      ValidatorError.InvalidSchema(
        NonEmptyList.of(
          ValidatorError.SchemaIssue("$.properties", "invalid type")
        )
      ),
      None
    )
    val shown = error.show
    (shown must contain("Resolved schema cannot be used to validate an instance")) and
      (shown must contain("invalid type")) and
      (shown must contain("$.properties"))
  }

  def e4 = {
    val error: ClientError = ClientError.ResolutionError(
      SortedMap(
        "Repo A" -> LookupHistory(Set(RegistryError.NotFound), 1, now)
      )
    )
    val shown = error.show
    (shown must contain("Schema cannot be resolved")) and
      (shown must contain("Repo A")) and
      (shown must contain("1 attempt"))
  }

  def e5 = {
    val error: ClientError = ClientError.ResolutionError(
      SortedMap(
        "Repo A" -> LookupHistory(
          Set(RegistryError.RepoFailure("timeout"), RegistryError.NotFound),
          3,
          now
        )
      )
    )
    val shown = error.show
    (shown must contain("3 attempts")) and
      (shown must contain("Repo A"))
  }

  def e6 = {
    val error: ClientError = ClientError.ResolutionError(
      SortedMap(
        "Test Repo" -> LookupHistory(Set(RegistryError.NotFound), 1, now)
      )
    )
    val msg = error.getMessage
    (msg must contain("ResolutionError")) and
      (msg must contain("Test Repo"))
  }

  def e7 = {
    val error = ClientError.ResolutionError(
      SortedMap(
        "Repo A" -> LookupHistory(Set(RegistryError.NotFound), 1, now),
        "Repo B" -> LookupHistory(Set(RegistryError.NotFound), 1, now)
      )
    )
    error.isNotFound must beTrue
  }

  def e8 = {
    val error = ClientError.ResolutionError(
      SortedMap(
        "Repo A" -> LookupHistory(Set(RegistryError.NotFound), 1, now),
        "Repo B" -> LookupHistory(Set(RegistryError.RepoFailure("boom")), 1, now)
      )
    )
    error.isNotFound must beFalse
  }
}
