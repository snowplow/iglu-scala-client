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

// Java
import java.net.URI

// Cats
import cats.syntax.either._
import cats.syntax.show._

// circe
import io.circe.{Decoder, DecodingFailure, HCursor, Json}

// Iglu Core
import com.snowplowanalytics.iglu.core.SelfDescribingSchema

/** ADT supporting all native (from resolver-config) registries */
sealed trait Registry extends Product with Serializable {

  /** Our configuration for this RepositoryRef */
  def config: Registry.Config

  /**
   * All repositories with a search priority of
   * 1 will be checked before any repository with a search priority of 2
   */
  def classPriority: Int
}

object Registry {

  /**
   * An embedded repository is one which is embedded inside the calling code,
   * e.g. inside the jar's resources folder
   */
  case class Embedded(config: Config, path: String) extends Registry {
    val classPriority = 2
  }

  /** HTTP repository, such as Iglu Server or Iglu Central */
  case class Http(config: Config, http: HttpConnection) extends Registry {
    val classPriority = 100
  }

  /** Repository where all schemas provided on initialization time */
  case class InMemory(config: Config, schemas: List[SelfDescribingSchema[Json]]) extends Registry {
    val classPriority = 1
  }

  /** Common config for RepositoryRef classes */
  case class Config(
    name: String,
    instancePriority: Int,
    vendorPrefixes: List[String]
  ) {

    /**
     * Helper to check if this repository should take priority because of a
     * vendor prefix match. Returns true if we matched our schema's vendor
     * in the list of vendor prefixes.
     *
     * @param vendor a schema's vendor to find among specified in resolver
     * @return whether this is a priority lookup or not
     */
    def vendorMatched(vendor: String): Boolean =
      vendorPrefixes.exists(vendor.startsWith)
  }

  /** Primary, public Snowplow-supported Iglu Registry */
  val IgluCentral = Http(
    Config("Iglu Central", 10, List("com.snowplowanalytics")),
    HttpConnection(URI.create("http://iglucentral.com"), None)
  )

  val EmbeddedRegistry = {
    val config = Registry.Config("Iglu Client Embedded", 0, Nil)
    Registry.Embedded(config, path = "/iglu-client-embedded")
  }

  /** Helper class to extract HTTP URI and api key from config JSON */
  case class HttpConnection(uri: URI, apikey: Option[String])

  private implicit val uriCirceJsonDecoder: Decoder[URI] =
    Decoder.instance { cursor =>
      for {
        string <- cursor.as[String]
        uri    <- Utils.stringToUri(string).leftMap(e => DecodingFailure(e.show, cursor.history))
      } yield uri
    }

  implicit val httpConnectionDecoder: Decoder[HttpConnection] =
    new Decoder[HttpConnection] {
      def apply(c: HCursor): Decoder.Result[HttpConnection] =
        for {
          uri    <- c.downField("uri").as[URI]
          apikey <- c.downField("apikey").as[Option[String]]
        } yield HttpConnection(uri, apikey)
    }

  implicit val repoRefConfigCirceDecoder: Decoder[Config] =
    Decoder.instance { cursor =>
      for {
        name           <- cursor.downField("name").as[String]
        priority       <- cursor.downField("priority").as[Int]
        vendorPrefixes <- cursor.downField("vendorPrefixes").as[List[String]]
      } yield Config(name, priority, vendorPrefixes)
    }

  /**
   * Builds a RepositoryRef sub-type from the given a Json.
   * Uses the connection property to determine which RepositoryRef to build
   *
   * Currently supports:
   * 1. EmbeddedRepositoryRef
   * 2. HttpRepositoryRef
   *
   * @param config The JSON containing the configuration for this repository
   * @return our constructed RepositoryRef
   */
  def parse(config: Json): Decoder.Result[Registry] =
    for {
      conf       <- config.as[Config]
      connection <- config.hcursor.downField("connection").as[Json]

      embedded = for {
        embedded <- connection.hcursor.downField("embedded").as[Json]
        path     <- embedded.hcursor.downField("path").as[String]
      } yield Embedded(conf, path)
      http = connection.hcursor.downField("http").as[HttpConnection].map(conn => Http(conf, conn))

      result <- embedded.orElse(http)
    } yield result

}
