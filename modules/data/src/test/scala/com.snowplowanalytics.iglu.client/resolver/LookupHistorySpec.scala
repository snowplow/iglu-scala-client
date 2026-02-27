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
package com.snowplowanalytics.iglu.client.resolver

import java.time.Instant

import io.circe.syntax._
import io.circe.literal._

import com.snowplowanalytics.iglu.client.resolver.registries.RegistryError

import org.specs2.Specification

class LookupHistorySpec extends Specification {

  val now: Instant     = Instant.ofEpochSecond(1562598285)
  val later: Instant   = Instant.ofEpochSecond(1562598385)
  val earlier: Instant = Instant.ofEpochSecond(1562598185)

  def is = s2"""
  LookupHistory should
    encode to JSON correctly                   $e1
    decode from JSON correctly                 $e2
    incrementAttempt should increase attempts   $e3
    Semigroup combine should merge errors       $e4
    Semigroup combine should take max attempts  $e5
    Semigroup combine should take max instant   $e6
    Semigroup combine should cap errors at MaxErrors $e7
  """

  def e1 = {
    val history = LookupHistory(Set(RegistryError.NotFound), 1, now)
    val json = json"""{
      "errors": [{"error": "NotFound"}],
      "attempts": 1,
      "lastAttempt": "2019-07-08T15:04:45Z"
    }"""
    history.asJson must beEqualTo(json)
  }

  def e2 = {
    val json = json"""{
      "errors": [{"error": "NotFound"}],
      "attempts": 2,
      "lastAttempt": "2019-07-08T15:04:45Z"
    }"""
    val expected = LookupHistory(Set(RegistryError.NotFound), 2, now)
    json.as[LookupHistory] must beRight(expected)
  }

  def e3 = {
    val history     = LookupHistory(Set(RegistryError.NotFound), 3, now)
    val incremented = history.incrementAttempt
    (incremented.attempts must_== 4) and
      (incremented.errors must_== history.errors) and
      (incremented.lastAttempt must_== history.lastAttempt)
  }

  def e4 = {
    val a        = LookupHistory(Set[RegistryError](RegistryError.NotFound), 1, now)
    val b        = LookupHistory(Set[RegistryError](RegistryError.RepoFailure("timeout")), 2, later)
    val combined = LookupHistory.lookupHistorySemigroup.combine(a, b)
    combined.errors must_== Set[RegistryError](
      RegistryError.NotFound,
      RegistryError.RepoFailure("timeout")
    )
  }

  def e5 = {
    val a        = LookupHistory(Set[RegistryError](RegistryError.NotFound), 1, now)
    val b        = LookupHistory(Set[RegistryError](RegistryError.NotFound), 5, now)
    val combined = LookupHistory.lookupHistorySemigroup.combine(a, b)
    combined.attempts must_== 5
  }

  def e6 = {
    val a        = LookupHistory(Set[RegistryError](RegistryError.NotFound), 1, earlier)
    val b        = LookupHistory(Set[RegistryError](RegistryError.NotFound), 1, later)
    val combined = LookupHistory.lookupHistorySemigroup.combine(a, b)
    combined.lastAttempt must_== later
  }

  def e7 = {
    val errors: Set[RegistryError] = (1 to LookupHistory.MaxErrors)
      .map(i => RegistryError.RepoFailure(s"error-$i"): RegistryError)
      .toSet
    val a        = LookupHistory(errors, 1, now)
    val b        = LookupHistory(Set[RegistryError](RegistryError.NotFound), 1, now)
    val combined = LookupHistory.lookupHistorySemigroup.combine(a, b)
    combined.errors.size must be_<=(LookupHistory.MaxErrors)
  }
}
