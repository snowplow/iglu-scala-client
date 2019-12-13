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
package com.snowplowanalytics.iglu.client.resolver
package registries

// Java
import java.net.UnknownHostException

// Scala
import scala.util.control.NonFatal

// cats
import cats.{Eval, Id}
import cats.data.{EitherT, OptionT}
import cats.effect.Sync
import cats.implicits._
import cats.effect.implicits._

// circe
import io.circe.Json
import io.circe.parser.parse

// Iglu Core
import com.snowplowanalytics.iglu.core.{SchemaKey, SchemaList, SelfDescribingSchema}
import com.snowplowanalytics.iglu.core.circe.instances._

/** A capability of `F` to communicate with Iglu registries, using `RepositoryRef` ADT,
 * in order to lookup for schemas or get schema lists
 *
 * @tparam F effect type, preferably referentially-transparent, but can be [[Id]]
 *           in case of distributed engine like Spark
 */
trait RegistryLookup[F[_]] {
  // This can be abstracted over RepositoryRef to let others extend functionality of Resolver

  /** Find a schema in the particular `RepositoryRef`
   *
   * @param registry one of supported repository types
   * @param schemaKey The SchemaKey uniquely identifying the schema in Iglu
   * @return either schema parsed into `Json` or `RegistryError`, such as absent schema,
   *         or unexpected response
   */
  def lookup(registry: Registry, schemaKey: SchemaKey): F[Either[RegistryError, Json]]

  /** List all schemas (formats and versions) for a given vendor/name pair in their
   * chronological order. It is up to Registry to build valid list
   *
   * @param registry one of supported repository types (only HTTP is supported)
   * @param vendor precise schema vendor
   * @param name schema name
   * @return some parsed `SchemaList` (order is trusted) or none in any unexpected case
   */
  def list(
    registry: Registry,
    vendor: String,
    name: String,
    model: Int): F[Either[RegistryError, SchemaList]]
}

object RegistryLookup {

  def apply[F[_]](implicit ev: RegistryLookup[F]): RegistryLookup[F] = ev

  implicit class LookupOps(val repositoryRef: Registry) extends AnyVal with Serializable {
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

      def list(
        registry: Registry,
        vendor: String,
        name: String,
        model: Int): F[Either[RegistryError, SchemaList]] =
        registry match {
          case Registry.Http(_, connection) => httpList(connection, vendor, name, model)
          case _                            => F.pure(RegistryError.NotFound.asLeft)
        }
    }

  // Non-RT instances. Use with caution

  implicit def evalLookupInstance: RegistryLookup[Eval] =
    new RegistryLookup[Eval] {
      def lookup(registry: Registry, schemaKey: SchemaKey): Eval[Either[RegistryError, Json]] =
        Eval.always(idLookupInstance.lookup(registry, schemaKey))
      def list(
        registry: Registry,
        vendor: String,
        name: String,
        model: Int): Eval[Either[RegistryError, SchemaList]] =
        Eval.always(idLookupInstance.list(registry, vendor, name, model))
    }

  // Id instance also swallows all exceptions into `RegistryError`
  implicit def idLookupInstance: RegistryLookup[Id] = new RegistryLookup[Id] {
    def lookup(repositoryRef: Registry, schemaKey: SchemaKey): Id[Either[RegistryError, Json]] =
      repositoryRef match {
        case Registry.Http(_, connection) =>
          Utils
            .stringToUri(toPath(connection.uri.toString, schemaKey))
            .flatMap(uri => Utils.unsafeGetFromUri(uri, connection.apikey))
        case Registry.Embedded(_, base) =>
          val path = toPath(base, schemaKey)
          Utils.unsafeEmbeddedLookup(path)
        case Registry.InMemory(_, schemas) =>
          inMemoryLookup(schemas, schemaKey)
      }

    def list(
      registry: Registry,
      vendor: String,
      name: String,
      model: Int): Id[Either[RegistryError, SchemaList]] =
      registry match {
        case Registry.Http(_, connection) =>
          val subpath = toSubpath(connection.uri.toString, vendor, name, model)
          Utils.stringToUri(subpath).flatMap(Utils.unsafeHttpList(_, connection.apikey))
        case _ =>
          RegistryError.NotFound.asLeft
      }
  }

  def inMemoryLookup(
    schemas: List[SelfDescribingSchema[Json]],
    key: SchemaKey): Either[RegistryError, Json] =
    schemas.find(s => s.self.schemaKey == key).toRight(RegistryError.NotFound).map(_.normalize)

  /** Common method to get an endpoint of `SchemaKey` */
  private[registries] def toPath(prefix: String, key: SchemaKey): String =
    s"${prefix.stripSuffix("/")}/schemas/${key.toPath}"

  private def toSubpath(prefix: String, vendor: String, name: String, model: Int): String =
    s"${prefix.stripSuffix("/")}/schemas/$vendor/$name/jsonschema/$model"

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

  private[registries] def httpList[F[_]: Sync](
    http: Registry.HttpConnection,
    vendor: String,
    name: String,
    model: Int): F[Either[RegistryError, SchemaList]] = {
    Utils
      .stringToUri(toSubpath(http.uri.toString, vendor, name, model))
      .traverse(uri => Utils.getFromUri(uri, http.apikey))
      .map { response =>
        for {
          body <- response
          text <- body.toRight(RegistryError.NotFound)
          json <- parse(text).leftMap(e => RegistryError.RepoFailure(e.show))
          list <- json.as[SchemaList].leftMap(e => RegistryError.RepoFailure(e.show))
        } yield list
      }
  }
}
