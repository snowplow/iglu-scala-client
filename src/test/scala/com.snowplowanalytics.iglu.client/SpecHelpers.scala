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
package com.snowplowanalytics.iglu.client

import java.net.URI
import java.util.concurrent.TimeUnit

// Cats
import cats.Id
import cats.effect.Clock
import cats.effect.IO

// This project
import resolver.registries.Registry
import validator.CirceValidator

object SpecHelpers {

  val IgluCentral: Registry =
    Registry.Http(
      Registry.Config("Iglu Central", 0, List("com.snowplowanalytics")),
      Registry.HttpConnection(URI.create("http://iglucentral.com"), None))

  val EmbeddedTest: Registry =
    Registry.Embedded(
      Registry.Config("Iglu Test Embedded", 0, List("com.snowplowanalytics")),
      path = "/iglu-test-embedded")

  // TODO: is it a very bad idea?
  implicit val idClock: Clock[Id] = new Clock[Id] {
    final def realTime(unit: TimeUnit): Id[Long] =
      unit.convert(System.currentTimeMillis(), TimeUnit.MILLISECONDS)

    final def monotonic(unit: TimeUnit): Id[Long] =
      unit.convert(System.nanoTime(), TimeUnit.NANOSECONDS)
  }

  // TODO: is it a very bad idea?
  implicit val ioClock: Clock[IO] = new Clock[IO] {
    final def realTime(unit: TimeUnit): IO[Long] =
      IO.delay(unit.convert(System.currentTimeMillis(), TimeUnit.MILLISECONDS))

    final def monotonic(unit: TimeUnit): IO[Long] =
      IO.delay(unit.convert(System.nanoTime(), TimeUnit.NANOSECONDS))
  }

  val TestResolver = Resolver.init[IO](cacheSize = 10, EmbeddedTest)
  val TestClient   = for { resolver <- TestResolver } yield Client(resolver, CirceValidator)

}
