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

import cats.effect.Sync
import cats.Id
import cats.data.OptionT
import cats.implicits._
import io.circe.Json
import io.circe.parser.parse

import com.snowplowanalytics.iglu.core.circe.implicits._
import com.snowplowanalytics.iglu.core.{SchemaKey, SchemaList}

import java.net.UnknownHostException
import java.net.URI
import java.net.http.HttpResponse.BodyHandlers
import java.net.http.{HttpClient, HttpRequest, HttpResponse}
import java.time.Duration

import scala.util.control.NonFatal

object JavaNetRegistryLookup {

  private val ReadTimeoutMs = 4000L

  private lazy val httpClient = HttpClient
    .newBuilder()
    .connectTimeout(Duration.ofMillis(1000))
    .build()

  implicit def ioLookupInstance[F[_]](implicit F: Sync[F]): RegistryLookup[F] =
    new RegistryLookup[F] {
      def lookup(repositoryRef: Registry, schemaKey: SchemaKey): F[Either[RegistryError, Json]] =
        repositoryRef match {
          case Registry.Http(_, connection) => httpLookup(connection, schemaKey)
          case Registry.Embedded(_, path)   => RegistryLookup.embeddedLookup[F](path, schemaKey)
          case Registry.InMemory(_, schemas) =>
            F.delay(RegistryLookup.inMemoryLookup(schemas, schemaKey))
        }

      def list(
        registry: Registry,
        vendor: String,
        name: String,
        model: Int
      ): F[Either[RegistryError, SchemaList]] =
        registry match {
          case Registry.Http(_, connection) => httpList(connection, vendor, name, model)
          case Registry.Embedded(_, base) =>
            val path = toSubpath(base, vendor, name)
            Sync[F].delay(Utils.unsafeEmbeddedList(path, model))
          case _ => F.pure(RegistryError.NotFound.asLeft)
        }
    }

  // Id instance also swallows all exceptions into `RegistryError`
  implicit def idLookupInstance: RegistryLookup[Id] =
    new RegistryLookup[Id] {
      def lookup(repositoryRef: Registry, schemaKey: SchemaKey): Id[Either[RegistryError, Json]] =
        repositoryRef match {
          case Registry.Http(_, connection) =>
            Utils
              .stringToUri(RegistryLookup.toPath(connection.uri.toString, schemaKey))
              .flatMap(uri => unsafeGetFromUri(uri, connection.apikey))
          case Registry.Embedded(_, base) =>
            val path = RegistryLookup.toPath(base, schemaKey)
            Utils.unsafeEmbeddedLookup(path)
          case Registry.InMemory(_, schemas) =>
            RegistryLookup.inMemoryLookup(schemas, schemaKey)
        }

      def list(
        registry: Registry,
        vendor: String,
        name: String,
        model: Int
      ): Id[Either[RegistryError, SchemaList]] =
        registry match {
          case Registry.Http(_, connection) =>
            val subpath = toSubpath(connection.uri.toString, vendor, name, model)
            Utils.stringToUri(subpath).flatMap(unsafeHttpList(_, connection.apikey))
          case Registry.Embedded(_, base) =>
            val path = toSubpath(base, vendor, name)
            Utils.unsafeEmbeddedList(path, model)
          case _ =>
            RegistryError.NotFound.asLeft
        }
    }

  /**
   * Retrieves an Iglu Schema from the HTTP Iglu Repo as a JSON
   *
   * @param http endpoint and optional apikey
   * @param key The SchemaKey uniquely identifying the schema in Iglu
   * @return either a `Json` on success, or `RegistryError` in case of any failure
   *         (i.e. all exceptions should be swallowed by `RegistryError`)
   */
  private def httpLookup[F[_]: Sync](
    http: Registry.HttpConnection,
    key: SchemaKey
  ): F[Either[RegistryError, Json]] =
    Utils
      .stringToUri(RegistryLookup.toPath(http.uri.toString, key))
      .traverse(uri => getFromUri(uri, http.apikey))
      .map { response =>
        val result = for {
          body <- OptionT(response)
          json = parse(body)
          result <- OptionT.liftF[Either[RegistryError, *], Json](
            json.leftMap(e => RegistryError.RepoFailure(e.show))
          )
        } yield result

        result.getOrElseF[Json](RegistryError.NotFound.asLeft)
      }
      .recover {
        case uhe: UnknownHostException =>
          val error = s"Unknown host issue fetching: ${uhe.getMessage}"
          RegistryError.RepoFailure(error).asLeft
        case NonFatal(nfe) =>
          val error = s"Unexpected exception fetching: $nfe"
          RegistryError.RepoFailure(error).asLeft
      }

  private def httpList[F[_]: Sync](
    http: Registry.HttpConnection,
    vendor: String,
    name: String,
    model: Int
  ): F[Either[RegistryError, SchemaList]] =
    Utils
      .stringToUri(toSubpath(http.uri.toString, vendor, name, model))
      .traverse(uri => getFromUri(uri, http.apikey))
      .map { response =>
        for {
          body <- response
          text <- body.toRight(RegistryError.NotFound)
          json <- parse(text).leftMap(e => RegistryError.RepoFailure(e.show))
          list <- json.as[SchemaList].leftMap(e => RegistryError.RepoFailure(e.show))
        } yield list
      }

  /**
   * Read a Json from an URI using optional apikey
   * with added optional header, so it is unsafe as well and throws same exceptions
   *
   * @param uri the URL to fetch the JSON document from
   * @param apikey optional apikey UUID to authenticate in Iglu Server
   * @return The document at that URL if code is 2xx
   */
  private def getFromUri[F[_]: Sync](uri: URI, apikey: Option[String]): F[Option[String]] =
    Sync[F].blocking(executeCall(uri, apikey))

  /** Non-RT analog of [[getFromUri]] */
  private def unsafeGetFromUri(uri: URI, apikey: Option[String]): Either[RegistryError, Json] =
    try {
      executeCall(uri, apikey)
        .map(parse)
        .map(_.leftMap(e => RegistryError.RepoFailure(e.show)))
        .getOrElse(RegistryError.NotFound.asLeft)
    } catch {
      case NonFatal(e) =>
        Utils.repoFailure(e).asLeft
    }

  /** Non-RT analog of [[JavaNetRegistryLookup.httpList]] */
  private def unsafeHttpList(uri: URI, apikey: Option[String]): Either[RegistryError, SchemaList] =
    for {
      json <- unsafeGetFromUri(uri, apikey)
      list <- json.as[SchemaList].leftMap(e => RegistryError.RepoFailure(e.show))
    } yield list

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

  private def toSubpath(
    prefix: String,
    vendor: String,
    name: String,
    model: Int
  ): String =
    s"${prefix.stripSuffix("/")}/schemas/$vendor/$name/jsonschema/$model"

  private def toSubpath(
    prefix: String,
    vendor: String,
    name: String
  ): String =
    s"${prefix.stripSuffix("/")}/schemas/$vendor/$name/jsonschema"


}
