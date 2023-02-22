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

import cats.Applicative
import com.snowplowanalytics.iglu.client.resolver.registries.{RegistryError, RegistryLookup}
import com.snowplowanalytics.iglu.core.{SchemaKey, SchemaList}
import io.circe.Json

import java.net.URI
import java.time.Instant
import java.util.concurrent.TimeUnit
import scala.concurrent.duration.FiniteDuration
import java.util.concurrent.atomic.AtomicReference

// Cats
import cats.Id
import cats.effect.Clock
import cats.effect.IO

// This project
import resolver.registries.Registry

object SpecHelpers {

  val now: Instant = Instant.ofEpochSecond(1562598285)

  def cleanTimestamps(error: ClientError): ClientError =
    error match {
      case ClientError.ResolutionError(value) =>
        ClientError.ResolutionError(value.map { case (k, v) => (k, v.copy(lastAttempt = now)) })
      case other => other
    }

  val IgluCentral: Registry =
    Registry.Http(
      Registry.Config("Iglu Central", 0, List("com.snowplowanalytics")),
      Registry.HttpConnection(URI.create("http://iglucentral.com"), None)
    )

  case class TrackingRegistry(
    lookupState: AtomicReference[List[String]],
    listState: AtomicReference[List[String]]
  ) extends RegistryLookup[IO] {
    override def lookup(
      registry: Registry,
      schemaKey: SchemaKey
    ): IO[Either[RegistryError, Json]] = {
      IO(
        lookupState.updateAndGet((l: List[String]) =>
          Seq(registry.config.name, schemaKey.toSchemaUri).mkString("-") :: l
        )
      ) >>
        RegistryLookup.ioLookupInstance[IO].lookup(registry, schemaKey)
    }

    override def list(
      registry: Registry,
      vendor: String,
      name: String,
      model: Int
    ): IO[Either[RegistryError, SchemaList]] = {
      IO(
        listState.updateAndGet((l: List[String]) =>
          Seq(registry.config.name, vendor, name, model.toString).mkString("-") :: l
        )
      ) >>
        RegistryLookup.ioLookupInstance[IO].list(registry, vendor, name, model)
    }
  }
  def mkTrackingRegistry: TrackingRegistry = TrackingRegistry(
    new AtomicReference[List[String]](List.empty[String]),
    new AtomicReference[List[String]](List.empty[String])
  )

  val EmbeddedTest: Registry =
    Registry.Embedded(
      Registry.Config("Iglu Test Embedded", 0, List("com.snowplowanalytics")),
      path = "/iglu-test-embedded"
    )

  implicit val idClock: Clock[Id] = new Clock[Id] {

    override def applicative: Applicative[Id] = Applicative[Id]

    override def monotonic: Id[FiniteDuration] =
      FiniteDuration(System.currentTimeMillis(), TimeUnit.MILLISECONDS)

    override def realTime: Id[FiniteDuration] =
      FiniteDuration(System.nanoTime(), TimeUnit.NANOSECONDS)

  }

  val TestResolver = Resolver.init[IO](10, None, EmbeddedTest)
  val TestClient   = for { resolver <- TestResolver } yield Client(resolver, CirceValidator)
  val CachingTestClient =
    TestResolver.flatMap(resolver => IgluCirceClient.fromResolver[IO](resolver, cacheSize = 10))
}
