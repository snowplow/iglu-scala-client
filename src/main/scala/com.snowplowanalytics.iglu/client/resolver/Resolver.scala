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
package com.snowplowanalytics.iglu.client.resolver

// cats
import cats.{Applicative, Monad, Id}
import cats.data._
import cats.effect.Clock
import cats.implicits._

// circe
import io.circe.{Decoder, DecodingFailure, HCursor, Json}

// Iglu Core
import com.snowplowanalytics.iglu.core.{SchemaCriterion, SchemaKey, SchemaVer, SelfDescribingData}
import com.snowplowanalytics.iglu.core.circe.CirceIgluCodecs._

// This project
import com.snowplowanalytics.iglu.client.ClientError.ResolutionError
import com.snowplowanalytics.iglu.client.resolver.registries.{
  Registry,
  RegistryError,
  RegistryLookup
}

/** Resolves schemas from one or more Iglu schema registries */
final case class Resolver[F[_]](repos: List[Registry], cache: Option[SchemaCache[F]]) {
  import Resolver._

  private[client] val allRepos: NonEmptyList[Registry] =
    NonEmptyList[Registry](Registry.EmbeddedRegistry, repos)

  /**
   * Tries to find the given schema in any of the provided repository refs
   * If any of repositories gives non-non-found error, lookup will retried
   *
   * @param schemaKey The SchemaKey uniquely identifying the schema in Iglu
   * @param attempts number of attempts to retry after non-404 errors
   *                 retries will happen in *next* requests
   * @return a Validation boxing either the Schema's
   *         Json on Success, or an error String
   *         on Failure
   */
  def lookupSchema(schemaKey: SchemaKey, attempts: Int)(
    implicit
    F: Monad[F],
    L: RegistryLookup[F],
    C: Clock[F]): F[Either[ResolutionError, Json]] = {
    val prioritize = prioritizeRepos(schemaKey, _: List[Registry]).toList
    def retryCached(serverErrors: LookupFailureMap): F[SchemaLookup] = {
      val reposForRetry = getReposForRetry(serverErrors, attempts)
      traverseRepos[F](schemaKey, prioritize(reposForRetry), serverErrors)
    }

    val resultAction = getFromCache(schemaKey).flatMap {
      case Some(lookupResult) => lookupResult.fold(retryCached, finish[F, Json])
      case None               => traverseRepos[F](schemaKey, prioritize(allRepos.toList), Map.empty)
    }

    for {
      result <- resultAction
      _      <- addToCache(schemaKey, result)
    } yield postProcess(result)
  }

  private def addToCache(schemaKey: SchemaKey, result: SchemaLookup)(
    implicit F: Monad[F],
    C: Clock[F]) =
    cache.traverse(c => c.store(schemaKey, result))

  private def getFromCache(
    schemaKey: SchemaKey)(implicit F: Monad[F], C: Clock[F]): F[Option[SchemaLookup]] =
    cache match {
      case Some(c) => c.get(schemaKey)
      case None    => Monad[F].pure(None)
    }
}

/** Companion object. Lets us create a Resolver from a Json */
object Resolver {

  /**
   * Tail-recursive function to find our schema in one of our repositories
   *
   * @param schemaKey The SchemaKey uniquely identifying
   *                  this schema in Iglu
   * @param remaining A List of repositories we have to look in
   *                  (not-tried yet or with non-404 error)
   * @param tried A Map of repositories with their accumulated errors
   *              we have looked in fruitlessly so far
   * @return either a Success-boxed schema (as a Json),
   *         or a Failure-boxing of Map of repositories with all their
   *         accumulated errors
   */
  def traverseRepos[F[_]: Monad: RegistryLookup](
    schemaKey: SchemaKey,
    remaining: List[Registry],
    tried: LookupFailureMap): F[SchemaLookup] = remaining match {
    case Nil => Applicative[F].pure(tried.asLeft)
    case repo :: tail =>
      RegistryLookup[F].lookup(repo, schemaKey).flatMap {
        case Right(schema) =>
          finish[F, Json](schema)
        case Left(e) => // TODO: we can pattern match here
          val error = LookupHistory(Set(e), 1, fatal = false)
          traverseRepos[F](schemaKey, tail, Map(repo -> error) |+| tried)
      }
  }

  private val ConfigurationSchema =
    SchemaCriterion("com.snowplowanalytics.iglu", "resolver-config", "jsonschema", 1, 0)

  val SelfDescribingKey =
    SchemaKey(
      "com.snowplowanalytics.self-desc",
      "instance-iglu-only",
      "jsonschema",
      SchemaVer.Full(1, 0, 0))

  /**
   * Constructs a Resolver instance from an arg array
   * of RepositoryRefs.
   *
   * @param cacheSize The size of the cache
   * @param refs Any RepositoryRef to add to this resolver
   * @return a configured Resolver instance
   */
  def init[F[_]: Applicative: InitCache](cacheSize: Int, refs: Registry*): F[Resolver[F]] =
    SchemaCache.init[F](cacheSize, None).map(cacheOpt => new Resolver(List(refs: _*), cacheOpt))

  /** Construct a pure resolver, working only with in-memory registries, no cache, no clock */
  def initPure(refs: Registry.InMemory*): Resolver[Id] =
    new Resolver[Id](List(refs: _*), None)

  // Keep this up-to-date
  private[client] val EmbeddedSchemaCount = 4

  /** A Resolver which only looks at our embedded repo */
  def bootstrap[F[_]: Applicative: InitCache]: F[Resolver[F]] =
    Resolver.init[F](EmbeddedSchemaCount, Registry.EmbeddedRegistry)

  private final case class ResolverConfig(
    cacheSize: Int,
    cacheTtl: Option[Int],
    repositoryRefs: List[Json])

  private implicit val resolverConfigCirceDecoder: Decoder[ResolverConfig] =
    new Decoder[ResolverConfig] {
      override final def apply(c: HCursor): Decoder.Result[ResolverConfig] = {
        for {
          cacheSize <- c.get[Int]("cacheSize")
          cacheTtl  <- c.get[Option[Int]]("cacheTtl")
          repos     <- c.get[List[Json]]("repositories")
        } yield ResolverConfig(cacheSize, cacheTtl, repos)
      }
    }

  /**
   * Construct a Resolver instance from a Json *and* validates
   * against embedded schema (hence side-effect)
   *
   * @param config The JSON containing the configuration
   *        for this resolver
   * @return a configured Resolver instance
   */
  def parse[F[_]: Monad: InitCache](config: Json): F[Either[DecodingFailure, Resolver[F]]] = {
    val result: EitherT[F, DecodingFailure, Resolver[F]] = for {
      datum    <- EitherT.fromEither[F](config.as[SelfDescribingData[Json]])
      _        <- matchConfig[F](datum)
      config   <- EitherT.fromEither[F](resolverConfigCirceDecoder(datum.data.hcursor))
      cacheOpt <- EitherT.liftF(SchemaCache.init[F](config.cacheSize, config.cacheTtl))
      refsE    <- EitherT.fromEither[F](config.repositoryRefs.traverse(Registry.parse))
      _        <- EitherT.fromEither[F](validateRefs(refsE))
    } yield Resolver(refsE, cacheOpt)

    result.value
  }

  private def finish[F[_], A](result: A)(
    implicit F: Applicative[F]): F[Either[LookupFailureMap, A]] =
    Applicative[F].pure(result.asRight[LookupFailureMap])

  /** Ensure that every names encountered only once */
  private[client] def validateRefs[F[_]](refs: List[Registry]): Either[DecodingFailure, Unit] =
    refs.map(_.config.name).groupBy(identity).mapValues(_.size).filter(_._2 > 1).toList match {
      case Nil =>
        ().asRight
      case some =>
        val report = some.map { case (name, times) => s"$name encountered $times times" }
        DecodingFailure(s"Repository names must be unique, ${report.mkString(", ")}", List.empty).asLeft
    }

  /**
   * Re-sorts RepositoryRefs into the optimal order for querying
   * (optimal = minimizing unsafe I/O).
   *
   * @param schemaKey SchemaKey uniquely identifying the schema in Iglu
   * @param repositoryRefs list of repository refs to be sorted
   * @return the prioritized List of RepositoryRefs.
   *         Pragmatically sorted to minimize lookups.
   */
  private[client] def prioritizeRepos(schemaKey: SchemaKey, repositoryRefs: List[Registry]) =
    repositoryRefs.sortBy(r =>
      (!r.config.vendorMatched(schemaKey), r.classPriority, r.config.instancePriority))

  /**
   * Get from Map of repository failures only those repository which
   * were failed with recoverable errors (like timeout or accidental server segfault)
   * AND retried less than `requiredAttempts` times
   *
   * @param failuresMap Map of repositories to their aggregated errors
   * @return repository refs which still need to be requested
   */
  private[client] def getReposForRetry[F[_]](
    failuresMap: LookupFailureMap,
    requiredAttempts: Int): List[Registry] = {
    val errorsToRetry = failuresMap.filter {
      case (_, LookupHistory(errors, attempts, unrecoverable)) =>
        attempts < requiredAttempts && !unrecoverable && !errors.contains(RegistryError.NotFound)
    }
    errorsToRetry.keys.toList
  }

  private def postProcess[F[_]](result: SchemaLookup) =
    result.leftMap { failure =>
      ResolutionError(failure.map { case (key, value) => (key.config.name, value) })
    }

  private def matchConfig[F[_]: Applicative](datum: SelfDescribingData[Json]) = {
    val failure =
      DecodingFailure(
        s"Schema ${datum.schema} does not match criterion ${ConfigurationSchema.asString}",
        List.empty)
    val matcher = Either.cond(ConfigurationSchema.matches(datum.schema), (), failure)
    EitherT.fromEither[F](matcher)
  }
}
