/*
 * Copyright (c) 2014-2021 Snowplow Analytics Ltd. All rights reserved.
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

// Scala
import scala.io.Source
import scala.util.control.NonFatal

// Cats
import cats.effect.Sync
import cats.syntax.either._
import cats.syntax.functor._
import cats.syntax.option._
import cats.syntax.show._

// circe
import io.circe.parser.parse
import io.circe.{Decoder, DecodingFailure, Json, ParsingFailure}

// Apache Commons
import org.apache.commons.lang3.exception.ExceptionUtils

// scalaj
import com.snowplowanalytics.iglu.core.SchemaList
import com.snowplowanalytics.iglu.core.circe.CirceIgluCodecs._
import scalaj.http._

private[registries] object Utils {

  val ConnectionTimeoutMs = 1000

  val ReadTimeoutMs = 4000

  /**
   * Read a Json from an URI using optional apikey
   * with added optional header, so it is unsafe as well and throws same exceptions
   *
   * @param uri the URL to fetch the JSON document from
   * @param apikey optional apikey UUID to authenticate in Iglu Server
   * @return The document at that URL if code is 2xx
   */
  def getFromUri[F[_]: Sync](uri: URI, apikey: Option[String]): F[Option[String]] =
    Sync[F]
      .delay(buildLookupRequest(uri, apikey).asString)
      .map { response =>
        if (response.is2xx) response.body.some else None
      }

  /** Non-RT analog of [[getFromUri]] */
  def unsafeGetFromUri(uri: URI, apikey: Option[String]): Either[RegistryError, Json] =
    try {
      val response = buildLookupRequest(uri, apikey).asString
      val data     = if (response.is2xx) response.body.some else none
      data
        .map(parse)
        .map(_.leftMap(e => RegistryError.RepoFailure(e.show)))
        .getOrElse(RegistryError.NotFound.asLeft)
    } catch {
      case NonFatal(e) =>
        repoFailure(e).asLeft
    }

  /** Not-RT analog of [[RegistryLookup.embeddedLookup]] */
  def unsafeEmbeddedLookup(path: String): Either[RegistryError, Json] =
    try {
      val is     = Utils.unsafeReadResource(path)
      val schema = is.map(unsafeFromStream)
      val result = schema
        .toRight(RegistryError.NotFound: RegistryError)
        .flatMap(x => parse(x).leftMap(invalidSchema))
      is.fold(())(unsafeCloseStream)
      result
    } catch {
      case NonFatal(e) =>
        repoFailure(e).asLeft
    }

  /** Non-RT analog of [[RegistryLookup.httpList]] */
  def unsafeHttpList(uri: URI, apikey: Option[String]): Either[RegistryError, SchemaList] =
    for {
      json <- unsafeGetFromUri(uri, apikey)
      list <- json.as[SchemaList].leftMap(e => RegistryError.RepoFailure(e.show))
    } yield list

  /**
   * A wrapper around Java's URI.
   *
   * @param url The String to convert to a URI
   * @return an URI, or an error message, all wrapped in an Either
   */
  def stringToUri(url: String): Either[RegistryError, URI] =
    try URI.create(url).asRight
    catch {
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

  private def buildLookupRequest(uri: URI, apikey: Option[String]): HttpRequest =
    apikey
      .map(key => Http(uri.toString).header("apikey", key))
      .getOrElse(Http(uri.toString))
      .timeout(ConnectionTimeoutMs, ReadTimeoutMs)

  private[resolver] def readResource[F[_]: Sync](path: String): F[Option[InputStream]] =
    Sync[F].delay(unsafeReadResource(path))
  private[resolver] def unsafeReadResource(path: String): Option[InputStream] =
    Option(getClass.getResource(path)).map(_.openStream())

  private[resolver] def fromStream[F[_]: Sync](is: InputStream): F[String] =
    Sync[F].delay(unsafeFromStream(is))
  private[resolver] def unsafeFromStream(is: InputStream): String =
    Source.fromInputStream(is).mkString

  private[resolver] def closeStream[F[_]: Sync](is: InputStream): F[Unit] =
    Sync[F].delay(unsafeCloseStream(is))
  private[resolver] def unsafeCloseStream(is: InputStream): Unit =
    is.close()

  private[resolver] def invalidSchema(failure: ParsingFailure): RegistryError =
    RegistryError.RepoFailure(failure.show)

  private[resolver] def repoFailure(failure: Throwable): RegistryError =
    RegistryError.RepoFailure(failure.getMessage)

}
