/*
 * Copyright (c) 2014-2018 Snowplow Analytics Ltd. All rights reserved.
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
package resolver.registries

// Java
import java.io.InputStream
import java.net.URI
import java.util.UUID

// Scala
import scala.io.Source

// Cats
import cats.data.OptionT
import cats.syntax.either._
import cats.syntax.option._
import cats.syntax.functor._
import cats.syntax.show._
import cats.effect.Sync

// circe
import io.circe.{Decoder, DecodingFailure, Json, ParsingFailure}
import io.circe.parser.parse

// Apache Commons
import org.apache.commons.lang3.exception.ExceptionUtils

// scalaj
import scalaj.http._

private[registries] object Utils {

  /**
   * Read a Json from an URI using optional apikey
   * with added optional header, so it is unsafe as well and throws same exceptions
   *
   * @param uri the URL to fetch the JSON document from
   * @param apikey optional apikey UUID to authenticate in Iglu Server
   * @return The document at that URL
   */
  def getFromUri[F[_]: Sync](uri: URI, apikey: Option[String]): F[Option[String]] = {
    val request = apikey
      .map(key => Http(uri.toString).header("apikey", key))
      .getOrElse(Http(uri.toString))

    Sync[F]
      .delay(request.asString)
      .map { response =>
        if (response.is2xx) response.body.some else None
      }
  }

  /**
   * A wrapper around Java's URI.
   *
   * @param url The String to convert to a URI
   * @return an URI, or an error message, all wrapped in an Either
   */
  def stringToUri(url: String): Either[RegistryError, URI] =
    try {
      URI.create(url).asRight
    } catch {
      case _: NullPointerException =>
        RegistryError.ClientFailure("Provided URL was null").asLeft
      case e: IllegalArgumentException =>
        val error = ExceptionUtils.getRootCause(e).getMessage
        RegistryError.ClientFailure(s"Provided URI string violates RFC 2396: [$error]").asLeft
    }

  implicit val uriCirceJsonDecoder: Decoder[URI] =
    Decoder.instance { cursor =>
      for {
        string <- cursor.as[String]
        uri    <- stringToUri(string).leftMap(e => DecodingFailure(e.show, cursor.history))
      } yield uri
    }

  private[resolver] def readResource[F[_]: Sync](path: String): F[Option[InputStream]] =
    Sync[F].delay(Option(getClass.getResource(path)).map(_.openStream()))

  private[resolver] def fromStream[F[_]: Sync](is: InputStream): F[String] =
    Sync[F].delay(Source.fromInputStream(is).mkString)

  private[resolver] def closeStream[F[_]: Sync](is: InputStream): F[Unit] =
    Sync[F].delay(is.close())

  private[resolver] def invalidSchema(failure: ParsingFailure): RegistryError =
    RegistryError.RepoFailure(failure.show)

  private[resolver] def repoFailure(failure: Throwable): RegistryError =
    RegistryError.RepoFailure(failure.getMessage)

}
