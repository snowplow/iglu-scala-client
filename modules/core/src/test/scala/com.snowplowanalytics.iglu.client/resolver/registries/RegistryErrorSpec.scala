/*
 * Copyright (c) 2014-2022 Snowplow Analytics Ltd. All rights reserved.
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

import io.circe.syntax._
import io.circe.literal._

import org.specs2.Specification

class RegistryErrorSpec extends Specification {
  def is = s2"""
  Encode and decode RegistryError $e1
  """

  def e1 = {
    val notFound: RegistryError      = RegistryError.NotFound
    val repoFailure: RegistryError   = RegistryError.RepoFailure("Repository went down")
    val clientFailure: RegistryError = RegistryError.ClientFailure("Something wrong on client side")

    val notFoundJson    = json"""{"error":"NotFound"}"""
    val repoFailureJson = json"""{"error":"RepoFailure","message":"Repository went down"}"""
    val clientFailureJson =
      json"""{"error":"ClientFailure","message":"Something wrong on client side"}"""

    val encodings =
      (notFound.asJson must beEqualTo(notFoundJson))
        .and(repoFailure.asJson must beEqualTo(repoFailureJson))
        .and(clientFailure.asJson must beEqualTo(clientFailureJson))

    val decodings =
      (notFoundJson.as[RegistryError] must beRight(notFound))
        .and(repoFailureJson.as[RegistryError] must beRight(repoFailure))
        .and(clientFailureJson.as[RegistryError] must beRight(clientFailure))

    encodings and decodings
  }

}
