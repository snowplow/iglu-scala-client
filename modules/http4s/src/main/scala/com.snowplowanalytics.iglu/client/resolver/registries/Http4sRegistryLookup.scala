/*
 * Copyright (c) 2021 Snowplow Analytics Ltd. All rights reserved.
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

import scala.util.control.NonFatal

import cats.implicits._
import cats.data.EitherT
import cats.effect.Sync

import io.circe.Json

import org.http4s.{EntityDecoder, Header, Headers, Method, Request, Status, Uri}
import org.http4s.circe._
import org.http4s.client.{Client => HttpClient}

import com.snowplowanalytics.iglu.core.{SchemaKey, SchemaList}
import com.snowplowanalytics.iglu.core.circe.CirceIgluCodecs._

object Http4sRegistryLookup {

  def apply[F[_]: Sync](client: HttpClient[F]): RegistryLookup[F] =
    new RegistryLookup[F] {
      def lookup(repositoryRef: Registry, schemaKey: SchemaKey): F[Either[RegistryError, Json]] =
        repositoryRef match {
          case Registry.Http(_, connection) => httpLookup(client, connection, schemaKey).value
          case Registry.Embedded(_, path)   => RegistryLookup.embeddedLookup[F](path, schemaKey)
          case Registry.InMemory(_, schemas) =>
            Sync[F].pure(RegistryLookup.inMemoryLookup(schemas, schemaKey))
        }

      def list(
        registry: Registry,
        vendor: String,
        name: String,
        model: Int
      ): F[Either[RegistryError, SchemaList]] =
        registry match {
          case Registry.Http(_, connection) =>
            httpList(client, connection, vendor, name, model).value
          case _ => Sync[F].pure(RegistryError.NotFound.asLeft)
        }
    }

  def httpLookup[F[_]: Sync](
    client: HttpClient[F],
    http: Registry.HttpConnection,
    key: SchemaKey
  ): EitherT[F, RegistryError, Json] =
    for {
      uri <- EitherT.fromEither[F](toPath(http, key))
      headers =
        http.apikey.fold[Headers](Headers.empty)(apikey => Headers.of(Header("apikey", apikey)))
      response =
        runRequest[F, Json](client, Request[F](method = Method.GET, uri = uri, headers = headers))
      result <- EitherT(response)
    } yield result

  def httpList[F[_]: Sync](
    client: HttpClient[F],
    http: Registry.HttpConnection,
    vendor: String,
    name: String,
    model: Int
  ): EitherT[F, RegistryError, SchemaList] =
    for {
      uri <- EitherT.fromEither[F](toSubpath(http, vendor, name, model))
      headers =
        http.apikey.fold[Headers](Headers.empty)(apikey => Headers.of(Header("apikey", apikey)))
      response = runRequest[F, SchemaList](
        client,
        Request[F](method = Method.GET, uri = uri, headers = headers)
      )
      result <- EitherT(response)
    } yield result

  def toPath(cxn: Registry.HttpConnection, key: SchemaKey): Either[RegistryError, Uri] =
    Uri
      .fromString(s"${cxn.uri.toString.stripSuffix("/")}/schemas/${key.toPath}")
      .leftMap(e => RegistryError.ClientFailure(e.message))

  def toSubpath(
    cxn: Registry.HttpConnection,
    vendor: String,
    name: String,
    model: Int
  ): Either[RegistryError, Uri] =
    Uri
      .fromString(s"${cxn.uri.toString.stripSuffix("/")}/schemas/$vendor/$name/jsonschema/$model")
      .leftMap(e => RegistryError.ClientFailure(e.message))

  def runRequest[F[_]: Sync, A: EntityDecoder[F, *]](
    client: HttpClient[F],
    req: Request[F]
  ): F[Either[RegistryError, A]] = {
    val responseResult = client.run(req).use[F, Either[RegistryError, A]] {
      case Status.Successful(response) =>
        response
          .as[A]
          .map(_.asRight[RegistryError])
          .handleError {
            case NonFatal(exception) =>
              RegistryError.ClientFailure(s"Could not decode server response. $exception").asLeft[A]
          }
      case Status.ClientError(response) if response.status.code == 404 =>
        (RegistryError.NotFound: RegistryError).asLeft[A].pure[F]
      case Status.ServerError(response) =>
        response.bodyText.compile.string.map { body =>
          val error = s"Unexpected server response: $body"
          RegistryError.RepoFailure(error).asLeft
        }
      case Status.ClientError(response) =>
        response.bodyText.compile.string.map { body =>
          val error = s"Unexpected server response: $body"
          RegistryError.ClientFailure(error).asLeft
        }
      case response =>
        response.bodyText.compile.string.map { body =>
          val error = s"Unexpected response: $body"
          RegistryError.ClientFailure(error).asLeft
        }
    }

    responseResult.recover {
      case NonFatal(exception) =>
        val error = Option(exception.getMessage).getOrElse(exception.toString)
        RegistryError.ClientFailure(error).asLeft
    }
  }

  implicit def schemaListDecoder[F[_]: Sync]: EntityDecoder[F, SchemaList] = jsonOf[F, SchemaList]
}
