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
package com.snowplowanalytics.iglu.client.resolver.registries

// Java
import java.net.UnknownHostException

// Scala
import scala.util.control.NonFatal

// cats
import cats.{Eval, Id}
import cats.data.{EitherT, OptionT}
import cats.effect.{IO, Sync}
import cats.implicits._
import cats.effect.implicits._

// circe
import io.circe.Json
import io.circe.parser.parse

// Iglu Core
import com.snowplowanalytics.iglu.core.{SchemaKey, SelfDescribingSchema}
import com.snowplowanalytics.iglu.core.circe.implicits._

/** A capability of `F` to perform a schema lookup action, using `RepositoryRef` ADT
 *
 * @tparam F effect type
 */
trait RegistryLookup[F[_]] {
  // This can be abstracted over RepositoryRef to let others extend functionality of Resolver

  /** Find a schema in the particular `RepositoryRef`
   *
   * @param repositoryRef one of supported repository types
   * @param schemaKey The SchemaKey uniquely identifying the schema in Iglu
   * @return either schema parsed into `Json` or `RegistryError`, such as absent schema,
   *         or unexpected response
   */
  def lookup(repositoryRef: Registry, schemaKey: SchemaKey): F[Either[RegistryError, Json]]
}

object RegistryLookup {

  def apply[F[_]](implicit ev: RegistryLookup[F]): RegistryLookup[F] = ev

  implicit class LookupOps(repositoryRef: Registry) {
    def lookupSchema[F[_]: RegistryLookup](schemaKey: SchemaKey): F[Either[RegistryError, Json]] =
      RegistryLookup[F].lookup(repositoryRef, schemaKey)
  }

  implicit def ioLookupInstance[F[_]](implicit F: Sync[F]): RegistryLookup[F] =
    new RegistryLookup[F] {
      def lookup(repositoryRef: Registry, schemaKey: SchemaKey): F[Either[RegistryError, Json]] =
        repositoryRef match {
          case Registry.Http(_, connection)  => httpLookup(connection, schemaKey)
          case Registry.Embedded(_, path)    => embeddedLookup[F](path, schemaKey)
          case Registry.InMemory(_, schemas) => F.pure(inMemoryLookup(schemas, schemaKey))
        }
    }

  // Non-RT instances. Use with caution

  implicit def evalLookupInstance: RegistryLookup[Eval] =
    new RegistryLookup[Eval] {
      def lookup(repositoryRef: Registry, schemaKey: SchemaKey): Eval[Either[RegistryError, Json]] =
        Eval.always(idLookupInstance.lookup(repositoryRef, schemaKey))
    }

  // Id instance also swallows all exceptions into `RegistryError`
  implicit def idLookupInstance: RegistryLookup[Id] = new RegistryLookup[Id] {
    def lookup(repositoryRef: Registry, schemaKey: SchemaKey): Id[Either[RegistryError, Json]] =
      repositoryRef match {
        case Registry.Http(_, connection) =>
          httpLookup[IO](connection, schemaKey).attemptT
            .leftMap(Utils.repoFailure)
            .value
            .unsafeRunSync()
            .flatten
        case Registry.Embedded(_, path) =>
          embeddedLookup[IO](path, schemaKey).attemptT
            .leftMap(Utils.repoFailure)
            .value
            .unsafeRunSync()
            .flatten
        case Registry.InMemory(_, schemas) =>
          inMemoryLookup(schemas, schemaKey)
      }
  }

  def inMemoryLookup(
    schemas: List[SelfDescribingSchema[Json]],
    key: SchemaKey): Either[RegistryError, Json] =
    schemas.find(s => s.self.schemaKey == key).toRight(RegistryError.NotFound).map(_.normalize)

  /** Common method to get an endpoint of `SchemaKey` */
  private def toPath(prefix: String, key: SchemaKey): String =
    s"${prefix.stripSuffix("/")}/schemas/${key.toPath}"

  /** Retrieves an Iglu Schema from the Embedded Iglu Repo as a JSON
   *
   * @param base path on the local filesystem system
   * @param key The SchemaKey uniquely identifying the schema in Iglu
   * @return either a `Json` on success, or `RegistryError` in case of any failure
   *         (i.e. all exceptions should be swallowed by `RegistryError`)
   */
  private[registries] def embeddedLookup[F[_]: Sync](
    base: String,
    key: SchemaKey): F[Either[RegistryError, Json]] = {
    val path   = toPath(base, key)
    val is     = Utils.readResource[F](path)
    val schema = is.bracket(_.traverse(Utils.fromStream[F]))(_.traverse_(Utils.closeStream[F]))
    val result = for {
      stringOption <- schema.attemptT.leftMap(Utils.repoFailure)
      string       <- EitherT.fromOption[F](stringOption, RegistryError.NotFound: RegistryError)
      json         <- EitherT.fromEither[F](parse(string)).leftMap(Utils.invalidSchema)
    } yield json

    result.value
  }

  /** Retrieves an Iglu Schema from the HTTP Iglu Repo as a JSON
   *
   * @param http endpoint and optional apikey
   * @param key The SchemaKey uniquely identifying the schema in Iglu
   * @return either a `Json` on success, or `RegistryError` in case of any failure
   *         (i.e. all exceptions should be swallowed by `RegistryError`)
   */
  private[registries] def httpLookup[F[_]: Sync](
    http: Registry.HttpConnection,
    key: SchemaKey): F[Either[RegistryError, Json]] = {
    Utils
      .stringToUri(toPath(http.uri.toString, key))
      .traverse(uri => Utils.getFromUri(uri, http.apikey))
      .map { response =>
        val result = for {
          body <- OptionT(response)
          json = parse(body)
          result <- OptionT.liftF[Either[RegistryError, ?], Json](json.leftMap(e =>
            RegistryError.RepoFailure(e.show)))
        } yield result

        result.getOrElseF[Json](RegistryError.NotFound.asLeft)
      }
      .handleError {
        case uhe: UnknownHostException =>
          val error = s"Unknown host issue fetching: ${uhe.getMessage}"
          RegistryError.RepoFailure(error).asLeft
        case NonFatal(nfe) =>
          val error = s"Unexpected exception fetching: $nfe"
          RegistryError.RepoFailure(error).asLeft
      }
  }
}
