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
package com.snowplowanalytics.iglu.client.resolver

// cats
import cats.{Applicative, Id, Monad}
import java.time.Instant

import scala.concurrent.duration.MILLISECONDS
import cats.data._
import cats.effect.Clock
import cats.implicits._
import io.circe.{Decoder, DecodingFailure, FailedCursor, HCursor, Json}
import com.snowplowanalytics.iglu.core.{
  SchemaCriterion,
  SchemaKey,
  SchemaList,
  SchemaMap,
  SchemaVer,
  SelfDescribingData,
  SelfDescribingSchema
}
import com.snowplowanalytics.iglu.core.circe.CirceIgluCodecs._
import com.snowplowanalytics.iglu.client.ClientError.ResolutionError
import com.snowplowanalytics.iglu.client.resolver.registries.{
  Registry,
  RegistryError,
  RegistryLookup
}
import com.snowplowanalytics.iglu.client.resolver.registries.Registry.Get
import com.snowplowanalytics.iglu.client.resolver.ResolverCache.TimestampedItem

import scala.collection.immutable.SortedMap

/** Resolves schemas from one or more Iglu schema registries */
final case class Resolver[F[_]](repos: List[Registry], cache: Option[ResolverCache[F]]) {
  import Resolver._

  private[client] val allRepos: NonEmptyList[Registry] =
    NonEmptyList[Registry](Registry.EmbeddedRegistry, repos)

  /**
   * Tries to find the given schema in any of the provided repository refs
   * If any of repositories gives non-non-found error, lookup will retried
   *
   * @param schemaKey The SchemaKey uniquely identifying the schema in Iglu
   * @param resolveSupersedingSchema Specify whether superseding schema version should be taken into account
   * @return a [[Resolver.ResolverResult]] boxing the schema Json on success, or a ResolutionError on failure
   */
  def lookupSchemaResult(
    schemaKey: SchemaKey,
    resolveSupersedingSchema: Boolean = false
  )(implicit
    F: Monad[F],
    L: RegistryLookup[F],
    C: Clock[F]
  ): F[Either[ResolutionError, SchemaLookupResult]] = {
    def extractSupersededBy(schema: Json): Either[RegistryError, SupersededBy] =
      schema.hcursor.downField("$supersededBy") match {
        case _: FailedCursor => None.asRight
        case c =>
          c.as[SchemaVer.Full]
            .bimap(
              e =>
                RegistryError.ClientFailure(
                  s"Error while trying to decode superseding version: ${e.toString()}"
                ),
              _.some
            )
      }

    def checkSupersedingVersion(
      schemaKey: SchemaKey,
      supersededBy: SupersededBy
    ): Either[RegistryError, Unit] =
      supersededBy match {
        case None => ().asRight
        case Some(superseding) =>
          if (Ordering[SchemaVer.Full].gt(superseding, schemaKey.version)) ().asRight
          else
            RegistryError
              .ClientFailure(
                s"Superseding version ${superseding.asString} isn't greater than the version of schema ${schemaKey.toPath}"
              )
              .asLeft
      }

    val get: Registry => F[Either[RegistryError, SchemaItem]] = {
      if (resolveSupersedingSchema)
        r =>
          (for {
            schema          <- EitherT(L.lookup(r, schemaKey))
            supersededByOpt <- EitherT.fromEither[F](extractSupersededBy(schema))
            _               <- EitherT.fromEither[F](checkSupersedingVersion(schemaKey, supersededByOpt))
            res <- supersededByOpt match {
              case None =>
                EitherT.rightT[F, RegistryError](SchemaItem(schema, Option.empty[SchemaVer.Full]))
              case Some(supersededBy) =>
                val supersedingSchemaKey = schemaKey.copy(version = supersededBy)
                EitherT(L.lookup(r, supersedingSchemaKey))
                  .map(supersedingSchema => SchemaItem(supersedingSchema, supersededBy.some))
            }
          } yield res).value
      else
        r => EitherT(L.lookup(r, schemaKey)).map(s => SchemaItem(s, Option.empty)).value
    }

    def handleAfterFetch(
      result: Either[LookupFailureMap, SchemaItem]
    ): F[Either[ResolutionError, SchemaLookupResult]] =
      cache match {
        case Some(c) =>
          c.putSchemaResult(schemaKey, result).map {
            case Right(ResolverCache.TimestampedItem(schema, timestamp)) =>
              Right(ResolverResult.Cached(schemaKey, schema, timestamp))
            case Left(failure) => Left(resolutionError(failure))
          }
        case None =>
          result
            .bimap[ResolutionError, SchemaLookupResult](
              resolutionError,
              ResolverResult.NotCached(_)
            )
            .pure[F]
      }

    getSchemaFromCache(schemaKey).flatMap {
      case Some(TimestampedItem(Right(schema), timestamp)) =>
        Monad[F].pure(Right(ResolverResult.Cached(schemaKey, schema, timestamp)))
      case Some(TimestampedItem(Left(failures), _)) =>
        retryCached[F, SchemaItem](get, schemaKey.vendor)(failures)
          .flatMap(handleAfterFetch)
      case None =>
        traverseRepos[F, SchemaItem](
          get,
          prioritize(schemaKey.vendor, allRepos.toList),
          Map.empty
        )
          .flatMap(handleAfterFetch)
    }
  }

  /**
   * Tries to find the given schema in any of the provided repository refs
   * If any of repositories gives non-non-found error, lookup will retried
   *
   * Similar to [[lookupSchemaResult]] but returns pure Json instead of full [[Resolver.ResolverResult]]
   * @param schemaKey The SchemaKey uniquely identifying the schema in Iglu
   * @return a Validation boxing either the Schema's
   *         Json on Success, or an error String
   *         on Failure
   */
  def lookupSchema(
    schemaKey: SchemaKey
  )(implicit
    F: Monad[F],
    L: RegistryLookup[F],
    C: Clock[F]
  ): F[Either[ResolutionError, Json]] =
    lookupSchemaResult(schemaKey).map(_.map(_.value.schema))

  /**
   * Vendor, name, model are extracted from supplied schema key to call on the `listSchemas`. The important difference
   * from `listSchemas` is that it would invalidate cache, if returned list did not contain SchemaKey supplied in
   * argument. Making it a safer option is latest schema bound is known.
   */
  def listSchemasLikeResult(schemaKey: SchemaKey)(implicit
    F: Monad[F],
    L: RegistryLookup[F],
    C: Clock[F]
  ): F[Either[ResolutionError, SchemaListLookupResult]] =
    listSchemasResult(schemaKey.vendor, schemaKey.name, schemaKey.version.model, Some(schemaKey))

  /**
   * Get list of available schemas for particular vendor and name part
   * Server supposed to return them in proper order
   */
  def listSchemasResult(vendor: String, name: String, model: Int)(implicit
    F: Monad[F],
    L: RegistryLookup[F],
    C: Clock[F]
  ): F[Either[ResolutionError, SchemaListLookupResult]] =
    listSchemasResult(vendor, name, model, None)

  /**
   * Get list of available schemas for particular vendor and name part
   * Has an extra argument `mustIncludeKey` which is used to invalidate cache if SchemaKey supplied in it is not in the
   * list.
   * Server supposed to return them in proper order
   */
  def listSchemasResult(
    vendor: String,
    name: String,
    model: Int,
    mustIncludeKey: Option[SchemaKey]
  )(implicit
    F: Monad[F],
    L: RegistryLookup[F],
    C: Clock[F]
  ): F[Either[ResolutionError, SchemaListLookupResult]] = {
    val get: Registry => F[Either[RegistryError, SchemaList]] = r => L.list(r, vendor, name, model)

    def handleAfterFetch(
      result: Either[LookupFailureMap, SchemaList]
    ): F[Either[ResolutionError, SchemaListLookupResult]] =
      cache match {
        case Some(c) =>
          c.putSchemaListResult(vendor, name, model, result).map {
            case Right(ResolverCache.TimestampedItem(schemaList, timestamp)) =>
              Right(ResolverResult.Cached((vendor, name, model), schemaList, timestamp))
            case Left(failure) => Left(resolutionError(failure))
          }
        case None =>
          result
            .bimap[ResolutionError, SchemaListLookupResult](
              resolutionError,
              ResolverResult.NotCached(_)
            )
            .pure[F]
      }

    getSchemaListFromCache(vendor, name, model).flatMap {
      case Some(TimestampedItem(Right(schemaList), timestamp)) =>
        if (mustIncludeKey.forall(schemaList.schemas.contains))
          Monad[F].pure(Right(ResolverResult.Cached((vendor, name, model), schemaList, timestamp)))
        else
          traverseRepos[F, SchemaList](get, prioritize(vendor, allRepos.toList), Map.empty)
            .flatMap(handleAfterFetch)
      case Some(TimestampedItem(Left(failures), _)) =>
        retryCached[F, SchemaList](get, vendor)(failures)
          .flatMap(handleAfterFetch)
      case None =>
        traverseRepos[F, SchemaList](get, prioritize(vendor, allRepos.toList), Map.empty)
          .flatMap(handleAfterFetch)
    }
  }

  /**
   * Get list of available schemas for particular vendor and name part
   * Server supposed to return them in proper order
   * Similar to `listSchemasResult` but return pure SchemaList instead of full [[Resolver.ResolverResult]]
   */
  def listSchemas(
    vendor: String,
    name: String,
    model: Int
  )(implicit
    F: Monad[F],
    L: RegistryLookup[F],
    C: Clock[F]
  ): F[Either[ResolutionError, SchemaList]] =
    listSchemasResult(vendor, name, model).map(_.map(_.value))

  /**
   * Vendor, name, model are extracted from supplied schema key to call on the `listSchemas`. The important difference
   * from `listSchemas` is that it would invalidate cache, if returned list did not contain SchemaKey supplied in
   * argument. Making it a safer option is latest schema bound is known.
   */
  def listSchemasLike(schemaKey: SchemaKey)(implicit
    F: Monad[F],
    L: RegistryLookup[F],
    C: Clock[F]
  ): F[Either[ResolutionError, SchemaList]] =
    listSchemasResult(schemaKey.vendor, schemaKey.name, schemaKey.version.model, Some(schemaKey))
      .map(_.map(_.value))

  /** Get list of full self-describing schemas available on Iglu Server for particular vendor/name pair */
  def fetchSchemas(
    vendor: String,
    name: String,
    model: Int
  )(implicit
    F: Monad[F],
    L: RegistryLookup[F],
    C: Clock[F]
  ): EitherT[F, ResolutionError, List[SelfDescribingSchema[Json]]] =
    for {
      list <- EitherT(listSchemas(vendor, name, model))
      result <- list.schemas.traverse { key =>
        EitherT(lookupSchema(key)).map(json => SelfDescribingSchema(SchemaMap(key), json))
      }
    } yield result

  private def getSchemaFromCache(
    schemaKey: SchemaKey
  )(implicit
    F: Monad[F],
    C: Clock[F]
  ): F[Option[ResolverCache.TimestampedItem[SchemaLookup]]] =
    cache match {
      case Some(c) => c.getTimestampedSchema(schemaKey)
      case None    => Monad[F].pure(None)
    }

  private def getSchemaListFromCache(
    vendor: String,
    name: String,
    model: Int
  )(implicit
    F: Monad[F],
    C: Clock[F]
  ): F[Option[ResolverCache.TimestampedItem[ListLookup]]] =
    cache match {
      case Some(c) => c.getTimestampedSchemaList(vendor, name, model)
      case None    => Monad[F].pure(None)
    }
}

/** Companion object. Lets us create a Resolver from a Json */
object Resolver {

  type SchemaListKey          = (String, String, Int) //vendor, name, model
  type SchemaLookupResult     = ResolverResult[SchemaKey, SchemaItem]
  type SchemaListLookupResult = ResolverResult[SchemaListKey, SchemaList]
  type SupersededBy           = Option[SchemaVer.Full]

  /**
   * The result of doing schema lookup
   * @param schema Schema json
   * @param supersededBy Superseding schema version if the schema is superseded by another schema.
   *                     Otherwise, it is None.
   */
  case class SchemaItem(schema: Json, supersededBy: SupersededBy)

  /** The result of doing a lookup with the resolver, carrying information on whether the cache was used */
  sealed trait ResolverResult[+K, +A] {
    def value: A
  }

  object ResolverResult {

    /**
     * The result of a lookup when the resolver is configured to use a cache
     *
     *  The timestamped value is helpful when the client code needs to perform an expensive
     *  calculation derived from the looked-up value. If the timestamp has not changed since a
     *  previous call, then the value is guaranteed to be the same as before, and the client code
     *  does not need to re-run the expensive calculation.
     *
     *  @param value the looked-up value
     *  @param timestamp epoch time in seconds of when the value was last cached by the resolver
     */
    case class Cached[K, A](key: K, value: A, timestamp: Int) extends ResolverResult[K, A]

    /** The result of a lookup when the resolver is not configured to use a cache */
    case class NotCached[A](value: A) extends ResolverResult[Nothing, A]
  }

  def retryCached[F[_]: Clock: Monad: RegistryLookup, A](
    get: Get[F, A],
    vendor: String
  )(
    cachedErrors: LookupFailureMap
  ): F[Either[LookupFailureMap, A]] =
    for {
      now <- Clock[F].realTime(MILLISECONDS).map(Instant.ofEpochMilli)
      reposForRetry = getReposForRetry(cachedErrors, now)
      result <- traverseRepos[F, A](get, prioritize(vendor, reposForRetry), cachedErrors)
    } yield result

  /**
   * Tail-recursive function to find our schema in one of our repositories
   *
   * @param get a function to get an entity from first registry
   * @param remaining A List of repositories we have to look in
   *                  (not-tried yet or with non-404 error)
   * @param tried A Map of repositories with their accumulated errors
   *              we have looked in fruitlessly so far
   * @return either a Success-boxed schema (as a Json),
   *         or a Failure-boxing of Map of repositories with all their
   *         accumulated errors
   */
  def traverseRepos[F[_]: Monad: RegistryLookup: Clock, A](
    get: Registry => F[Either[RegistryError, A]],
    remaining: List[Registry],
    tried: LookupFailureMap
  ): F[Either[LookupFailureMap, A]] =
    remaining match {
      case Nil => Applicative[F].pure(tried.asLeft)
      case repo :: tail =>
        get(repo).flatMap {
          case Right(list) =>
            finish[F, A](list)
          case Left(e) =>
            for {
              timestamp <- Clock[F].realTime(MILLISECONDS).map(Instant.ofEpochMilli)
              combinedMap = Map(repo -> LookupHistory(Set(e), 0, timestamp)) |+| tried
              failureMap  = updateMap[Registry, LookupHistory](combinedMap, repo, _.incrementAttempt)
              result <- traverseRepos[F, A](get, tail, failureMap |+| tried)
            } yield result
        }
    }

  private def updateMap[K, V](
    map: Map[K, V],
    k: K,
    f: V => V
  ): Map[K, V] =
    map.get(k).map(f) match {
      case Some(v) => map.updated(k, v)
      case None    => map
    }

  private val ConfigurationSchema =
    SchemaCriterion("com.snowplowanalytics.iglu", "resolver-config", "jsonschema", 1, 0)

  val SelfDescribingKey =
    SchemaKey(
      "com.snowplowanalytics.self-desc",
      "instance-iglu-only",
      "jsonschema",
      SchemaVer.Full(1, 0, 0)
    )

  /**
   * Constructs a Resolver instance from an arg array
   * of RepositoryRefs.
   *
   * @param cacheSize The size of the cache
   * @param cacheTtl Optional time to live for schemas
   * @param refs Any RepositoryRef to add to this resolver
   * @return a configured Resolver instance
   */
  def init[F[_]: Monad: InitSchemaCache: InitListCache](
    cacheSize: Int,
    cacheTtl: Option[Int],
    refs: Registry*
  ): F[Resolver[F]] =
    ResolverCache
      .init[F](cacheSize, cacheTtl)
      .map(cacheOpt => new Resolver(List(refs: _*), cacheOpt))

  /** Construct a pure resolver, working only with in-memory registries, no cache, no clock */
  def initPure(refs: Registry.InMemory*): Resolver[Id] =
    new Resolver[Id](List(refs: _*), None)

  // Keep this up-to-date
  private[client] val EmbeddedSchemaCount = 4

  /** A Resolver which only looks at our embedded repo */
  def bootstrap[F[_]: Monad: InitSchemaCache: InitListCache]: F[Resolver[F]] =
    Resolver.init[F](EmbeddedSchemaCount, None, Registry.EmbeddedRegistry)

  final case class ResolverConfig(
    cacheSize: Int,
    cacheTtl: Option[Int],
    repositoryRefs: List[Json]
  )

  private implicit val resolverConfigCirceDecoder: Decoder[ResolverConfig] =
    new Decoder[ResolverConfig] {
      override final def apply(c: HCursor): Decoder.Result[ResolverConfig] =
        for {
          cacheSize <- c.get[Int]("cacheSize")
          cacheTtl  <- c.get[Option[Int]]("cacheTtl")
          repos     <- c.get[List[Json]]("repositories")
        } yield ResolverConfig(cacheSize, cacheTtl, repos)
    }

  /**
   * Construct a Resolver instance from a Json *and* validates
   * against embedded schema (hence side-effect)
   *
   * @param config The JSON containing the configuration
   *        for this resolver
   * @return a configured Resolver instance
   */
  def parse[F[_]: Monad: InitSchemaCache: InitListCache](
    config: Json
  ): F[Either[DecodingFailure, Resolver[F]]] = {
    val result: EitherT[F, DecodingFailure, Resolver[F]] = for {
      config   <- EitherT.fromEither[F](parseConfig(config))
      resolver <- fromConfig[F](config)
    } yield resolver

    result.value
  }

  def parseConfig(
    config: Json
  ): Either[DecodingFailure, ResolverConfig] = {
    for {
      datum  <- config.as[SelfDescribingData[Json]]
      _      <- matchConfig(datum)
      config <- resolverConfigCirceDecoder(datum.data.hcursor)
    } yield config
  }

  def fromConfig[F[_]: Monad: InitSchemaCache: InitListCache](
    config: ResolverConfig
  ): EitherT[F, DecodingFailure, Resolver[F]] = {
    for {
      cacheOpt <- EitherT.liftF(ResolverCache.init[F](config.cacheSize, config.cacheTtl))
      refsE    <- EitherT.fromEither[F](config.repositoryRefs.traverse(Registry.parse))
      _        <- EitherT.fromEither[F](validateRefs(refsE))
    } yield Resolver(refsE, cacheOpt)
  }

  private def finish[F[_], A](
    result: A
  )(implicit
    F: Applicative[F]
  ): F[Either[LookupFailureMap, A]] =
    Applicative[F].pure(result.asRight[LookupFailureMap])

  /** Ensure that every names encountered only once */
  private[client] def validateRefs[F[_]](refs: List[Registry]): Either[DecodingFailure, Unit] =
    refs.map(_.config.name).groupBy(identity).filter(_._2.size > 1).toList match {
      case Nil =>
        ().asRight
      case some =>
        val report = some.map { case (name, times) => s"$name encountered $times times" }
        DecodingFailure(
          s"Repository names must be unique, ${report.mkString(", ")}",
          List.empty
        ).asLeft
    }

  /**
   * Re-sorts RepositoryRefs into the optimal order for querying
   * (optimal = minimizing unsafe I/O).
   *
   * @param vendor vendor of a schema
   * @param repositoryRefs list of repository refs to be sorted
   * @return the prioritized List of RepositoryRefs.
   *         Pragmatically sorted to minimize lookups.
   */
  private[client] def prioritize(vendor: String, repositoryRefs: List[Registry]) =
    repositoryRefs.sortBy(r =>
      (!r.config.vendorMatched(vendor), r.classPriority, r.config.instancePriority)
    )

  /**
   * Get from Map of repository failures only those repository which
   * were failed with recoverable errors (like timeout or accidental server segfault)
   *
   * @param failuresMap Map of repositories to their aggregated errors
   * @return repository refs which still need to be requested
   */
  private[client] def getReposForRetry(
    failuresMap: LookupFailureMap,
    now: Instant
  ): List[Registry] = {
    val errorsToRetry = failuresMap.filter {
      case (_: Registry.Embedded | _: Registry.InMemory, _) =>
        false
      case (_, LookupHistory(_, attempts, lastAttempt)) =>
        now.toEpochMilli - lastAttempt.toEpochMilli > backoff(attempts)
    }
    errorsToRetry.keys.toList
  }

  private def resolutionError(failure: LookupFailureMap): ResolutionError =
    ResolutionError(SortedMap[String, LookupHistory]() ++ failure.map {
      case (key, value) => (key.config.name, value)
    })

  private def matchConfig(datum: SelfDescribingData[Json]): Either[DecodingFailure, Unit] = {
    val failure =
      DecodingFailure(
        s"Schema ${datum.schema} does not match criterion ${ConfigurationSchema.asString}",
        List.empty
      )
    Either.cond(ConfigurationSchema.matches(datum.schema), (), failure)
  }

  // Minimum backoff period for retry
  private val MinBackoff = 500 // ms

  // Count how many milliseconds the Resolver needs to wait before retrying
  private def backoff(retryCount: Int): Long =
    retryCount match {
      case c if c > 20 => 1200000L + (retryCount * 100L)
      case c if c > 12 =>
        MinBackoff + Math.pow(2, retryCount.toDouble).toLong + 5000L
      case c if c > 8 =>
        MinBackoff + Math.pow(3, retryCount.toDouble).toLong
      case _ =>
        MinBackoff + Math.pow(4, retryCount.toDouble).toLong
    }
}
