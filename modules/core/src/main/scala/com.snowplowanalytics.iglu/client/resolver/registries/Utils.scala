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
package com.snowplowanalytics.iglu.client
package resolver.registries

// Java
import com.snowplowanalytics.iglu.core.{SchemaKey, SchemaVer}

import java.io.{File, InputStream}
import java.net.URI
import java.net.http.HttpResponse.BodyHandlers
import java.net.http.{HttpClient, HttpRequest, HttpResponse}
import java.time.Duration
import scala.util.matching.Regex

// Scala
import scala.io.Source
import scala.util.control.NonFatal

// Cats
import cats.effect.Sync
import cats.syntax.either._
import cats.syntax.option._
import cats.syntax.show._
import cats.syntax.traverse._

// circe
import io.circe.parser.parse
import io.circe.{Decoder, DecodingFailure, Json, ParsingFailure}

// Apache Commons
import org.apache.commons.lang3.exception.ExceptionUtils

// scalaj
import com.snowplowanalytics.iglu.core.SchemaList
import com.snowplowanalytics.iglu.core.circe.CirceIgluCodecs._

private[registries] object Utils {
  private val ReadTimeoutMs = 4000L

  private lazy val httpClient = HttpClient
    .newBuilder()
    .connectTimeout(Duration.ofMillis(1000))
    .build()

  /**
   * Read a Json from an URI using optional apikey
   * with added optional header, so it is unsafe as well and throws same exceptions
   *
   * @param uri the URL to fetch the JSON document from
   * @param apikey optional apikey UUID to authenticate in Iglu Server
   * @return The document at that URL if code is 2xx
   */
  def getFromUri[F[_]: Sync](uri: URI, apikey: Option[String]): F[Option[String]] =
    Sync[F].blocking(executeCall(uri, apikey))

  /** Non-RT analog of [[getFromUri]] */
  def unsafeGetFromUri(uri: URI, apikey: Option[String]): Either[RegistryError, Json] =
    try {
      executeCall(uri, apikey)
        .map(parse)
        .map(_.leftMap(e => RegistryError.RepoFailure(e.show)))
        .getOrElse(RegistryError.NotFound.asLeft)
    } catch {
      case NonFatal(e) =>
        repoFailure(e).asLeft
    }

  def unsafeEmbeddedList(path: String, modelMatch: Int): Either[RegistryError, SchemaList] =
    try {
      val d =
        new File(
          getClass.getResource(path).getPath
        ) // this will throw NPE for missing entry in embedded repos
      val schemaFileRegex: Regex = (".*/schemas/?" + // path to file
        "([a-zA-Z0-9-_.]+)/" +            // Vendor
        "([a-zA-Z0-9-_]+)/" +             // Name
        "([a-zA-Z0-9-_]+)/" +             // Format
        "([1-9][0-9]*)-(\\d+)-(\\d+)$").r // MODEL, REVISION and ADDITION

      def getFolderContent(d: File): List[String] = {
        d.listFiles
          .filter(_.isFile)
          .toList
          .filter(_.getName.startsWith(s"${modelMatch.toString}-"))
          .map(_.getAbsolutePath)
      }

      val content =
        if (d.exists & d.isDirectory)
          getFolderContent(d)
        else
          List.empty[String]

      content
        .traverse {
          case schemaFileRegex(vendor, name, format, model, revision, addition)
              if model == modelMatch.toString =>
            SchemaKey(
              vendor = vendor,
              name = name,
              format = format,
              version = SchemaVer
                .Full(model = model.toInt, revision = revision.toInt, addition = addition.toInt)
            ).asRight
          case f => RegistryError.RepoFailure(s"Corrupted schema file name at $f").asLeft
        }
        .map(_.sortBy(_.version))
        .flatMap(s =>
          if (s.isEmpty)
            RegistryError.NotFound.asLeft
          else
            s.asRight
        )
        .map(SchemaList.parseUnsafe)
    } catch {
      case NonFatal(e) =>
        e match {
          case _: NullPointerException => RegistryError.NotFound.asLeft
          case _                       => repoFailure(e).asLeft
        }
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
        e match {
          case _: NullPointerException => RegistryError.NotFound.asLeft
          case _                       => repoFailure(e).asLeft
        }
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

  private def executeCall(uri: URI, apikey: Option[String]): Option[String] = {
    val httpRequest = buildLookupRequest(uri, apikey)
    val response    = httpClient.send(httpRequest, BodyHandlers.ofString())
    if (is2xx(response)) response.body.some else None
  }

  private def buildLookupRequest(uri: URI, apikey: Option[String]): HttpRequest = {
    val baseRequest = HttpRequest
      .newBuilder(uri)
      .timeout(Duration.ofMillis(ReadTimeoutMs))

    apikey
      .fold(baseRequest)(key => baseRequest.header("apikey", key))
      .build()
  }

  private def is2xx(response: HttpResponse[String]) =
    response.statusCode() >= 200 && response.statusCode() <= 299

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
    RegistryError.RepoFailure(
      if (failure.getMessage != null) failure.getMessage else "Unhandled error"
    )
}
