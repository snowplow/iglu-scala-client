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
package com.snowplowanalytics.iglu.client.resolver.registries

import java.net.URI
import io.circe.ParsingFailure

import org.specs2.Specification

class UtilsSpec extends Specification {
  def is = s2"""
  Utils should
    parse valid URL to URI                      $e1
    return ClientFailure for null URL           $e2
    return ClientFailure for invalid URL        $e3
    invalidSchema should create RepoFailure     $e4
    repoFailure should create RepoFailure from exception $e5
    repoFailure should handle null message      $e6
  """

  def e1 = {
    val result = Utils.stringToUri("http://example.com/schemas")
    result must beRight(URI.create("http://example.com/schemas"))
  }

  def e2 = {
    val result = Utils.stringToUri(null)
    result must beLeft[RegistryError].which(_.isInstanceOf[RegistryError.ClientFailure])
  }

  def e3 = {
    val result = Utils.stringToUri("http://exa mple.com")
    result must beLeft[RegistryError].which(_.isInstanceOf[RegistryError.ClientFailure])
  }

  def e4 = {
    val failure = ParsingFailure("exhausted input", new RuntimeException("parse error"))
    val result  = Utils.invalidSchema(failure)
    result must beAnInstanceOf[RegistryError.RepoFailure]
  }

  def e5 = {
    val exception = new RuntimeException("Connection refused")
    val result    = Utils.repoFailure(exception)
    result must beAnInstanceOf[RegistryError.RepoFailure]
    result.asInstanceOf[RegistryError.RepoFailure].message must_== "Connection refused"
  }

  def e6 = {
    val exception = new RuntimeException(null: String)
    val result    = Utils.repoFailure(exception)
    result.asInstanceOf[RegistryError.RepoFailure].message must_== "Unhandled error"
  }
}
