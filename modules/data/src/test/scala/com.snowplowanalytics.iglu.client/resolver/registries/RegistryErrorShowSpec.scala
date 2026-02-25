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

import cats.syntax.show._

import org.specs2.Specification

class RegistryErrorShowSpec extends Specification {
  def is = s2"""
  RegistryError Show instance should
    show NotFound               $e1
    show RepoFailure            $e2
    show ClientFailure          $e3
  """

  def e1 = {
    val error: RegistryError = RegistryError.NotFound
    error.show must_== "NotFound"
  }

  def e2 = {
    val error: RegistryError = RegistryError.RepoFailure("Connection timeout")
    error.show must_== "Iglu Repository Failure. Connection timeout"
  }

  def e3 = {
    val error: RegistryError = RegistryError.ClientFailure("Bad configuration")
    error.show must_== "Iglu Client Failure. Bad configuration"
  }
}
