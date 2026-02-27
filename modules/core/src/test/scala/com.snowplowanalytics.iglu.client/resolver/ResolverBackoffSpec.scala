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

import com.snowplowanalytics.iglu.client.resolver.registries.{Registry, RegistryError}

import org.specs2.Specification

class ResolverBackoffSpec extends Specification {
  def is = s2"""
  Resolver.getReposForRetry should
    not retry if NotFound is among errors                  $noRetryNotFound
    not retry embedded repos even with non-NotFound        $noRetryEmbedded
    not retry InMemory repos even with non-NotFound        $noRetryInMemory
    retry HTTP repos with RepoFailure after backoff        $retryHttpAfterBackoff
    not retry HTTP repos before backoff expires            $noRetryBeforeBackoff
    retry HTTP repos with empty errors (expired TTL)       $retryOnEmptyErrors
  Resolver.prioritize should
    sort repos with vendor match first                     $vendorMatchFirst
    sort by classPriority second                           $classPrioritySecond
    sort by instancePriority third                         $instancePriorityThird
  """

  val httpRegistry = Registry.Http(
    Registry.Config("HTTP Repo", 10, List("com.snowplowanalytics")),
    Registry.HttpConnection(java.net.URI.create("http://example.com"), None)
  )

  val embeddedRegistry = Registry.Embedded(
    Registry.Config("Embedded Repo", 0, Nil),
    "/test"
  )

  val inMemoryRegistry = Registry.InMemory(
    Registry.Config("InMemory Repo", 0, Nil),
    Nil
  )

  val now     = Instant.now()
  val longAgo = Instant.ofEpochMilli(0)

  def noRetryNotFound = {
    val failures: LookupFailureMap = Map(
      httpRegistry -> LookupHistory(Set(RegistryError.NotFound), 1, longAgo)
    )
    Resolver.getReposForRetry(failures, now) must beEmpty
  }

  def noRetryEmbedded = {
    val failures: LookupFailureMap = Map(
      embeddedRegistry -> LookupHistory(
        Set(RegistryError.RepoFailure("error")),
        1,
        longAgo
      )
    )
    Resolver.getReposForRetry(failures, now) must beEmpty
  }

  def noRetryInMemory = {
    val failures: LookupFailureMap = Map(
      inMemoryRegistry -> LookupHistory(
        Set(RegistryError.RepoFailure("error")),
        1,
        longAgo
      )
    )
    Resolver.getReposForRetry(failures, now) must beEmpty
  }

  def retryHttpAfterBackoff = {
    // With longAgo as lastAttempt, sufficient time has passed
    val failures: LookupFailureMap = Map(
      httpRegistry -> LookupHistory(
        Set(RegistryError.RepoFailure("timeout")),
        1,
        longAgo
      )
    )
    Resolver.getReposForRetry(failures, now) must contain(httpRegistry)
  }

  def noRetryBeforeBackoff = {
    // With now as lastAttempt, not enough time has passed
    val failures: LookupFailureMap = Map(
      httpRegistry -> LookupHistory(
        Set(RegistryError.RepoFailure("timeout")),
        1,
        now
      )
    )
    Resolver.getReposForRetry(failures, now) must beEmpty
  }

  def retryOnEmptyErrors = {
    // Empty errors means TTL expired; should retry
    val failures: LookupFailureMap = Map(
      httpRegistry -> LookupHistory(Set.empty, 0, longAgo)
    )
    Resolver.getReposForRetry(failures, now) must contain(httpRegistry)
  }

  def vendorMatchFirst = {
    val matched = Registry.Embedded(
      Registry.Config("Matched", 100, List("com.snowplowanalytics")),
      "/m"
    )
    val unmatched = Registry.Embedded(
      Registry.Config("Unmatched", 0, Nil),
      "/u"
    )
    val result = Resolver.prioritize("com.snowplowanalytics.iglu", List(unmatched, matched))
    result.head must_== matched
  }

  def classPrioritySecond = {
    val inMem    = Registry.InMemory(Registry.Config("InMem", 10, Nil), Nil)     // classPriority 1
    val embedded = Registry.Embedded(Registry.Config("Embedded", 10, Nil), "/e") // classPriority 2
    val http = Registry.Http(
      Registry.Config("Http", 10, Nil),
      Registry.HttpConnection(java.net.URI.create("http://example.com"), None)
    ) // classPriority 100
    val result = Resolver.prioritize("com.other", List(http, embedded, inMem))
    result must_== List(inMem, embedded, http)
  }

  def instancePriorityThird = {
    val low    = Registry.Embedded(Registry.Config("Low", 5, Nil), "/l")
    val high   = Registry.Embedded(Registry.Config("High", 100, Nil), "/h")
    val result = Resolver.prioritize("com.other", List(high, low))
    result must_== List(low, high)
  }
}
