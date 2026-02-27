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

import java.net.URI
import java.time.Instant

import cats.effect.IO
import cats.effect.testing.specs2.CatsEffect

import com.snowplowanalytics.iglu.client.ClientError.ResolutionError
import com.snowplowanalytics.iglu.client.resolver.registries.{Registry, RegistryError}

import scala.collection.immutable.SortedMap

import org.specs2.Specification

class ResolverErrorSpec extends Specification with CatsEffect {
  def is = s2"""
  Resolver.isNotFound should
    return true when all custom repos have NotFound             $isNotFoundAllNotFound
    return true when Iglu Central has NotFound among errors     $isNotFoundIgluCentralNotFound
    return false when custom has RepoFailure only               $isNotFoundCustomRepoFailure
    return true when custom has ClientFailure only              $isNotFoundCustomClientFailure
    return true when Iglu Central has ClientFailure             $isNotFoundIgluCentralClientFailure
    return true when empty (no repos)                           $isNotFoundEmpty
  Resolver.isSystemError should
    return true when custom has only RepoFailure                $isSystemCustomRepoFailure
    return true when custom has only ClientFailure              $isSystemCustomClientFailure
    return false when custom has NotFound with RepoFailure      $isSystemCustomNotFoundWithFailure
    return true when all Iglu Central mirrors have fatal errors $isSystemIgluCentralFatal
    return false when Iglu Central has NotFound                 $isSystemIgluCentralNotFound
    return false when no repos have errors                      $isSystemEmpty
  """

  val now = Instant.now()

  val igluCentralRegistry = Registry.Http(
    Registry.Config("Iglu Central", 0, List("com.snowplowanalytics")),
    Registry.HttpConnection(URI.create("http://iglucentral.com"), None)
  )

  val igluCentralMirror = Registry.Http(
    Registry.Config("Iglu Central Mirror", 10, List("com.snowplowanalytics")),
    Registry.HttpConnection(URI.create("http://mirror01.iglucentral.com"), None)
  )

  val customRegistry = Registry.Http(
    Registry.Config("Custom", 5, Nil),
    Registry.HttpConnection(URI.create("http://custom.example.com"), None)
  )

  def makeResolver(repos: List[Registry]): Resolver[IO] =
    Resolver[IO](repos, None)

  def makeError(histories: Map[String, LookupHistory]): ResolutionError =
    ResolutionError(SortedMap[String, LookupHistory]() ++ histories)

  def isNotFoundAllNotFound = {
    val resolver = makeResolver(List(customRegistry))
    val error = makeError(
      Map("Custom" -> LookupHistory(Set(RegistryError.NotFound), 1, now))
    )
    resolver.isNotFound(error) must beTrue
  }

  def isNotFoundIgluCentralNotFound = {
    val resolver = makeResolver(List(igluCentralRegistry, customRegistry))
    val error = makeError(
      Map(
        "Iglu Central" -> LookupHistory(
          Set(RegistryError.NotFound, RegistryError.RepoFailure("timeout")),
          1,
          now
        ),
        "Custom" -> LookupHistory(Set(RegistryError.NotFound), 1, now)
      )
    )
    resolver.isNotFound(error) must beTrue
  }

  def isNotFoundCustomRepoFailure = {
    val resolver = makeResolver(List(customRegistry))
    val error = makeError(
      Map("Custom" -> LookupHistory(Set(RegistryError.RepoFailure("timeout")), 1, now))
    )
    resolver.isNotFound(error) must beFalse
  }

  def isNotFoundCustomClientFailure = {
    val resolver = makeResolver(List(customRegistry))
    val error = makeError(
      Map("Custom" -> LookupHistory(Set(RegistryError.ClientFailure("bad config")), 1, now))
    )
    resolver.isNotFound(error) must beTrue
  }

  def isNotFoundIgluCentralClientFailure = {
    val resolver = makeResolver(List(igluCentralRegistry))
    val error = makeError(
      Map(
        "Iglu Central" -> LookupHistory(
          Set(RegistryError.ClientFailure("config error")),
          1,
          now
        )
      )
    )
    resolver.isNotFound(error) must beTrue
  }

  def isNotFoundEmpty = {
    val resolver = makeResolver(Nil)
    val error    = makeError(Map.empty)
    resolver.isNotFound(error) must beTrue
  }

  def isSystemCustomRepoFailure = {
    val resolver = makeResolver(List(customRegistry))
    val error = makeError(
      Map("Custom" -> LookupHistory(Set(RegistryError.RepoFailure("500 Server Error")), 1, now))
    )
    resolver.isSystemError(error) must beTrue
  }

  def isSystemCustomClientFailure = {
    val resolver = makeResolver(List(customRegistry))
    val error = makeError(
      Map("Custom" -> LookupHistory(Set(RegistryError.ClientFailure("DNS failure")), 1, now))
    )
    resolver.isSystemError(error) must beTrue
  }

  def isSystemCustomNotFoundWithFailure = {
    val resolver = makeResolver(List(customRegistry))
    val error = makeError(
      Map(
        "Custom" -> LookupHistory(
          Set(RegistryError.NotFound, RegistryError.RepoFailure("timeout")),
          1,
          now
        )
      )
    )
    resolver.isSystemError(error) must beFalse
  }

  def isSystemIgluCentralFatal = {
    val resolver = makeResolver(List(igluCentralRegistry, igluCentralMirror))
    val error = makeError(
      Map(
        "Iglu Central"        -> LookupHistory(Set(RegistryError.RepoFailure("timeout")), 1, now),
        "Iglu Central Mirror" -> LookupHistory(Set(RegistryError.RepoFailure("timeout")), 1, now)
      )
    )
    resolver.isSystemError(error) must beTrue
  }

  def isSystemIgluCentralNotFound = {
    val resolver = makeResolver(List(igluCentralRegistry))
    val error = makeError(
      Map("Iglu Central" -> LookupHistory(Set(RegistryError.NotFound), 1, now))
    )
    resolver.isSystemError(error) must beFalse
  }

  def isSystemEmpty = {
    val resolver = makeResolver(Nil)
    val error    = makeError(Map.empty)
    resolver.isSystemError(error) must beFalse
  }
}
