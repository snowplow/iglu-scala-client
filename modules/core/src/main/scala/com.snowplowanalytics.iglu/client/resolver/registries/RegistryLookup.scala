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
package com.snowplowanalytics.iglu.client.resolver
package registries

// cats
import cats.effect.Sync
import cats.implicits._
import cats.ApplicativeThrow

// circe
import io.circe.Json

// Iglu Core
import com.snowplowanalytics.iglu.core.circe.implicits._
import com.snowplowanalytics.iglu.core.{SchemaKey, SchemaList, SelfDescribingSchema}

import scala.util.control.NonFatal

/**
 * A capability of `F` to communicate with Iglu registries, using `RepositoryRef` ADT,
 * in order to lookup for schemas or get schema lists
 *
 * @tparam F effect type, preferably referentially-transparent, but can be `Id`
 *           in case of distributed engine like Spark
 */
trait RegistryLookup[F[_]] {
  // This can be abstracted over RepositoryRef to let others extend functionality of Resolver

  /**
   * Find a schema in the particular `RepositoryRef`
   *
   * @param registry one of supported repository types
   * @param schemaKey The SchemaKey uniquely identifying the schema in Iglu
   * @return either schema parsed into `Json` or `RegistryError`, such as absent schema,
   *         or unexpected response
   */
  def lookup(registry: Registry, schemaKey: SchemaKey): F[Either[RegistryError, Json]]

  /**
   * List all schemas (formats and versions) for a given vendor/name pair in their
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
    model: Int
  ): F[Either[RegistryError, SchemaList]]
}

object RegistryLookup {

  def apply[F[_]](implicit ev: RegistryLookup[F]): RegistryLookup[F] = ev

  implicit class LookupOps(val repositoryRef: Registry) extends AnyVal with Serializable {
    def lookupSchema[F[_]: RegistryLookup](schemaKey: SchemaKey): F[Either[RegistryError, Json]] =
      RegistryLookup[F].lookup(repositoryRef, schemaKey)

    def list[F[_]: RegistryLookup](
      vendor: String,
      name: String,
      model: Int
    ): F[Either[RegistryError, SchemaList]] =
      RegistryLookup[F].list(repositoryRef, vendor: String, name: String, model: Int)
  }

  /**
   * An implementation of [[RegistryLookup]] with standard implementations of embedded/in-memory
   *  lookups and pluggable implementation of http lookups
   */
  private[registries] abstract class StdRegistryLookup[F[_]: Sync] extends RegistryLookup[F] {

    /** Abstract methods to be provided by the implementation */
    def httpLookup(registry: Registry.Http, schemaKey: SchemaKey): F[Either[RegistryError, Json]]
    def httpList(
      registry: Registry.Http,
      vendor: String,
      name: String,
      model: Int
    ): F[Either[RegistryError, SchemaList]]

    /** Common functionality across all implementations */
    override def lookup(registry: Registry, schemaKey: SchemaKey): F[Either[RegistryError, Json]] =
      registry match {
        case http: Registry.Http =>
          withErrorHandling {
            httpLookup(http, schemaKey)
          }
        case Registry.Embedded(_, path) => Embedded.lookup[F](path, schemaKey)
        case Registry.InMemory(_, schemas) =>
          Sync[F].delay(RegistryLookup.inMemoryLookup(schemas, schemaKey))
      }

    override def list(
      registry: Registry,
      vendor: String,
      name: String,
      model: Int
    ): F[Either[RegistryError, SchemaList]] =
      registry match {
        case http: Registry.Http =>
          withErrorHandling {
            httpList(http, vendor, name, model)
          }
        case Registry.Embedded(_, base) =>
          val path = toSubpath(base, vendor, name)
          Sync[F].delay(Embedded.unsafeList(path, model))
        case _ => Sync[F].pure(RegistryError.NotFound.asLeft)
      }

  }

  private def withErrorHandling[F[_]: ApplicativeThrow, A](
    f: F[Either[RegistryError, A]]
  ): F[Either[RegistryError, A]] =
    f.recover { case NonFatal(nfe) =>
      val error = s"Unexpected exception fetching: $nfe"
      RegistryError.RepoFailure(error).asLeft
    }

  private[registries] def inMemoryLookup(
    schemas: List[SelfDescribingSchema[Json]],
    key: SchemaKey
  ): Either[RegistryError, Json] =
    schemas.find(s => s.self.schemaKey == key).toRight(RegistryError.NotFound).map(_.normalize)

  /** Common method to get an endpoint of `SchemaKey` */
  private[registries] def toPath(prefix: String, key: SchemaKey): String =
    s"${prefix.stripSuffix("/")}/schemas/${key.toPath}"

  private[registries] def toSubpath(
    prefix: String,
    vendor: String,
    name: String,
    model: Int
  ): String =
    s"${prefix.stripSuffix("/")}/schemas/$vendor/$name/jsonschema/$model"

  private[registries] def toSubpath(
    prefix: String,
    vendor: String,
    name: String
  ): String =
    s"${prefix.stripSuffix("/")}/schemas/$vendor/$name/jsonschema"

}
