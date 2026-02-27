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

import cats.effect.IO
import cats.effect.testing.specs2.CatsEffect

import io.circe.literal._

import org.specs2.Specification

class ClientParseSpec extends Specification with CatsEffect {
  def is = s2"""
  Client.parseDefault should
    successfully parse a valid resolver config        $parseValid
    return error for invalid resolver config          $parseInvalid
  IgluCirceClient.parseDefault should
    successfully parse a valid resolver config        $cachingParseValid
    return error for invalid resolver config          $cachingParseInvalid
  IgluCirceClient.fromResolver should
    create a client from an existing resolver         $fromResolver
  """

  val validConfig = json"""{
    "schema": "iglu:com.snowplowanalytics.iglu/resolver-config/jsonschema/1-0-3",
    "data": {
      "cacheSize": 10,
      "repositories": [
        {
          "name": "Iglu Test Embedded",
          "priority": 0,
          "vendorPrefixes": [ "com.snowplowanalytics" ],
          "connection": {
            "embedded": {
              "path": "/iglu-test-embedded"
            }
          }
        }
      ]
    }
  }"""

  val invalidConfig = json"""{"not": "a resolver config"}"""

  def parseValid =
    Client.parseDefault[IO](validConfig).value.map { result =>
      result must beRight
    }

  def parseInvalid =
    Client.parseDefault[IO](invalidConfig).value.map { result =>
      result must beLeft
    }

  def cachingParseValid =
    IgluCirceClient.parseDefault[IO](validConfig, Int.MaxValue).value.map { result =>
      result must beRight
    }

  def cachingParseInvalid =
    IgluCirceClient.parseDefault[IO](invalidConfig, Int.MaxValue).value.map { result =>
      result must beLeft
    }

  def fromResolver =
    for {
      resolver <- resolver.Resolver.init[IO](10, None, SpecHelpers.EmbeddedTest)
      client   <- IgluCirceClient.fromResolver[IO](resolver, 10, 40)
    } yield client.resolver must_== resolver
}
