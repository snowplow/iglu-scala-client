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
import cats.data.EitherT
import cats.effect.implicits._
import cats.implicits._

// circe
import io.circe.Json
import io.circe.parser.parse

// Iglu Core
import com.snowplowanalytics.iglu.core.circe.implicits._
import com.snowplowanalytics.iglu.core.{SchemaKey, SchemaList, SelfDescribingSchema}

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

  def inMemoryLookup(
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

  /**
   * Retrieves an Iglu Schema from the Embedded Iglu Repo as a JSON
   *
   * @param base path on the local filesystem system
   * @param key The SchemaKey uniquely identifying the schema in Iglu
   * @return either a `Json` on success, or `RegistryError` in case of any failure
   *         (i.e. all exceptions should be swallowed by `RegistryError`)
   */
  private[registries] def embeddedLookup[F[_]: Sync](
    base: String,
    key: SchemaKey
  ): F[Either[RegistryError, Json]] = {
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

}
