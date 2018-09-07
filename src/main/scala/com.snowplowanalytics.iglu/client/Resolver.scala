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
package com.snowplowanalytics.iglu.client

// Scala
import com.snowplowanalytics.iglu.client.utils.ValidationExceptions
import io.circe.Decoder.Result
import io.circe.{AccumulatingDecoder, DecodingFailure, HCursor}

import scala.annotation.tailrec
import scala.concurrent.ExecutionContext.Implicits.global

// Cats
import cats.Semigroup
import cats.implicits._
import cats.data.{EitherT, NonEmptyList}
import cats.data.Validated._
import cats.effect.Sync
import cats.effect.{IO, LiftIO}

// circe
import io.circe.{Decoder, Json}
import io.circe.syntax._
import io.circe.optics.JsonPath._

// LruMap
import com.snowplowanalytics.lrumap.LruMap

// Iglu Core
import com.snowplowanalytics.iglu.core.{SchemaCriterion, SchemaKey}

// This project
import utils.SchemaKeyUtils
import repositories.{EmbeddedRepositoryRef, HttpRepositoryRef, RepositoryRef}
import validation.SchemaValidation.{getErrors, isValid}
import validation.ValidatableCirceMethods
import utils.JacksonCatsUtils._
import validation.ProcessingMessage
import validation.ProcessingMessageMethods._

/**
 * Companion object. Lets us create a Resolver from
 * a Json
 */
object Resolver {

  private val ConfigurationSchema =
    SchemaCriterion("com.snowplowanalytics.iglu", "resolver-config", "jsonschema", 1, 0)

  /**
   * Helper class responsible for aggregating repository lookup errors
   * Using to aggregate all errors for single schema for single repo during all retries
   *
   * @param errors set of all errors happened during all attempts
   * @param attempts amount of undertaken attempts
   * @param unrecoverable indicates whether among failures were unrecoverable ones (like invalid schema)
   */
  private[client] case class RepoError(
    errors: Set[ProcessingMessage],
    attempts: Int,
    unrecoverable: Boolean)

  /**
   * Semigroup instance helping to aggregate repository errors
   */
  private[client] implicit object RepoErrorSemigroup extends Semigroup[RepoError] {
    override def combine(a: RepoError, b: RepoError): RepoError =
      RepoError(
        a.errors |+| b.errors,
        a.attempts.max(b.attempts) + 1,
        a.unrecoverable || b.unrecoverable)
  }

  /**
   * Constructs a Resolver instance from an arg array
   * of RepositoryRefs.
   *
   * @param cacheSize The size of the cache
   * @param refs Any RepositoryRef to add to this
   *        resolver
   * @return a configured Resolver instance
   */
  def apply[F[_]: Sync](cacheSize: Int, refs: RepositoryRef*): F[Resolver[F]] =
    SchemaCache[F](cacheSize).map(cacheOpt => new Resolver(cacheOpt, List(refs: _*)))

  final case class ResolverConfig(cacheSize: Int, cacheTtl: Option[Int], repositoryRefs: List[Json])

  val resolverConfigDecoder: AccumulatingDecoder[ResolverConfig] =
    new Decoder[ResolverConfig] {
      override final def apply(c: HCursor): Result[ResolverConfig] = {
        for {
          cacheSize <- c.downField("cacheSize").as[Int]
          cacheTtl  <- c.downField("cacheTtl").as[Option[Int]]
          repos     <- c.downField("repositories").as[List[Json]]
        } yield ResolverConfig(cacheSize, cacheTtl, repos)
      }
    }.accumulating

  /**
   * Constructs a Resolver instance from a Json.
   *
   * @param config The JSON containing the configuration
   *        for this resolver
   * @return a configured Resolver instance
   */
  def parse[F[_]: Sync](config: Json): F[Either[NonEmptyList[ProcessingMessage], Resolver[F]]] = {
    import ValidatableCirceMethods._
    import ValidationExceptions._

    // We can use the bootstrap Resolver for working
    // with JSON Schemas here.
    val resolver = Bootstrap.resolver[F]

    resolver.flatMap { implicit resolver =>
      val result = for {
        json <- EitherT(config.verifySchemaAndValidate(ConfigurationSchema, dataOnly = true))
        config <- EitherT
          .fromEither(resolverConfigDecoder(json.hcursor).toEither)
          .leftMap(nel => nel.map(failure => ProcessingMessage(failure.getMessage)))
        cacheOpt <- EitherT.liftF(SchemaCache(config.cacheSize, config.cacheTtl))
        refs     <- EitherT.fromEither(getRepositoryRefs(config.repositoryRefs).toEither)
      } yield Resolver(cacheOpt, refs)

      result.value
    }
  }

  /**
   * Extracts a List of RepositoryRefs from the
   * given a json.
   *
   * @param repositoryConfigs The JSON containing
   *        all of the repository configurations
   * @return our assembled List of RepositoryRefs
   */
  private[client] def getRepositoryRefs(
    repositoryConfigs: List[Json]): ValidatedNelType[RepositoryRefs] =
    repositoryConfigs.map { conf =>
      buildRepositoryRef(conf)
    }.sequence

  /**
   * Builds a RepositoryRef sub-type from the
   * given a Json. Uses the connection property
   * to determine which RepositoryRef to build.
   *
   * Currently supports:
   * 1. EmbeddedRepositoryRef
   * 2. HttpRepositoryRef
   *
   * @param repositoryConfig The JSON containing the
   *        configuration for this repository
   * @return our constructed RepositoryRef
   */
  private[client] def buildRepositoryRef(
    repositoryConfig: Json): ValidatedNelType[RepositoryRef] = {
    val rc = repositoryConfig
    if (EmbeddedRepositoryRef.isEmbedded(rc)) {
      EmbeddedRepositoryRef.parse(rc)
    } else if (HttpRepositoryRef.isHttp(rc)) {
      HttpRepositoryRef.parse(rc)
    } else {
      s"Configuration unrecognizable as either embedded or HTTP repository".invalid.toProcessingMessageNel
    }
  }

  /**
   * Re-sorts RepositoryRefs into the optimal order for querying
   * (optimal = minimizing unsafe I/O).
   *
   * @param schemaKey SchemaKey uniquely identifying
   *        the schema in Iglu
   * @param repositoryRefs list of repository refs to be sorted
   * @return the prioritized List of RepositoryRefs.
   *         Pragmatically sorted to minimize lookups.
   */
  private[client] def prioritizeRepos(schemaKey: SchemaKey, repositoryRefs: RepositoryRefs) =
    repositoryRefs.sortBy(r =>
      (!r.vendorMatched(schemaKey), r.classPriority, r.config.instancePriority))

  /**
   * Get from Map of repository failures only those repository which
   * were failed with recoverable errors (like timeout or accidental server segfault)
   * AND retried less than `requiredAttempts` times
   *
   * @param failuresMap Map of repositories to their aggregated errors
   * @return repository refs which still need to be requested
   */
  private[client] def getReposForRetry(
    failuresMap: RepoFailuresMap,
    requiredAttempts: Int): RepositoryRefs = {
    val errorsToRetry = failuresMap.filter {
      case (_, Some(RepoError(_, attempts, unrecoverable))) =>
        attempts < requiredAttempts && !unrecoverable
      case (_, None) => false
    }
    errorsToRetry.keys.toList
  }

  /**
   * Collects together the errors for a failed lookup into a NonEmptyList
   * There always be at least notFound error
   *
   * @param schemaKey The SchemaKey uniquely identifying this schema in Iglu
   * @param tried a map of all tried repositories with their accumulated errors
   * @return a NonEmptyList of ProcessingMessages
   */
  private[client] def collectErrors(
    schemaKey: SchemaKey,
    tried: RepoFailuresMap): ProcessingMessageNel = {
    val failures = tried.toList
      .collect { case (_, Some(error)) => error }
      .flatMap { _.errors.toList }

    // TODO: consider adding amount of undertaken attempts to message
    val repos =
      prioritizeRepos(schemaKey, tried.keys.toList).reverse // TODO: remove this legacy order, pre-0.4.0 errors were queued LIFO
        .map(repo => s"${repo.config.name} [${repo.descriptor}]")

    val notFound = ProcessingMessage(
      message = s"Could not find schema with key ${schemaKey.toSchemaUri} in any repository, tried:",
      repositories = Some(repos.asJson)
    )

    NonEmptyList(notFound, failures)
  }
}

/**
 * Resolves schemas from one or more Iglu schema
 * repositories.
 *
 * This is an extremely primitive implementation.
 * Currently it only supports lookups of schemas
 * specified by the exact same version (i.e.
 * MODEL-REVISION-ADDITION).
 */
case class Resolver[F[_]: Sync](
  cache: Option[SchemaCache[F]],
  repos: List[RepositoryRef],
) {
  import Resolver._

  private[client] val allRepos = Bootstrap.Repo :: repos

  /**
   * Tries to find the given schema in any of the
   * provided repository refs.
   * If any of repositories gives non-non-found error,
   * lookup will retried 3 times
   *
   * @param schemaKey The SchemaKey uniquely identifying
   *        the schema in Iglu
   * @param attempts number of attempts to retry after non-404 errors
   * @return a Validation boxing either the Schema's
   *         Json on Success, or an error String
   *         on Failure
   */
  def lookupSchema(
    schemaKey: SchemaKey,
    attempts: Int = 3): F[Either[NonEmptyList[ProcessingMessage], Json]] = {
    val result = cache.flatTraverse(_.get(schemaKey)).flatMap {
      case Some(schema) =>
        schema
          .leftMap { serverErrors =>
            val reposForRetry = getReposForRetry(serverErrors, attempts)
            traverseRepos(
              schemaKey,
              Resolver.prioritizeRepos(schemaKey, reposForRetry),
              serverErrors)
          }
          .bitraverse(identity, Sync[F].pure(_))
          .map(_.leftFlatMap(identity))
      case None => traverseRepos(schemaKey, prioritizeRepos(schemaKey, allRepos), Map())
    }

    result
      .flatTap(lookup => cache.traverse(_.store(schemaKey, lookup)))
      .map(_.leftMap(collectErrors(schemaKey, _)))
  }

  /**
   * Tries to find the given schema in any of the
   * provided repository refs.
   *
   * Convenience function which converts an
   * Iglu-format schema URI to a SchemaKey to
   * perform the lookup.
   *
   * @param schemaUri The Iglu-format schema URI
   * @return a Validation boxing either the Schema's
   *         Json on Success, or an error String
   *         on Failure
   */
  def lookupSchema(schemaUri: String): F[Either[NonEmptyList[ProcessingMessage], Json]] =
    SchemaKeyUtils
      .parse(schemaUri)
      .leftMap(NonEmptyList.one)
      .flatTraverse(key => lookupSchema(key))

  /**
   * Tail-recursive function to find our schema in one
   * of our repositories.
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
  private def traverseRepos(
    schemaKey: SchemaKey,
    remaining: RepositoryRefs,
    tried: RepoFailuresMap): F[SchemaLookup] = {
    remaining match {
      case Nil => Sync[F].pure(tried.asLeft)
      case repo :: repos =>
        repo.lookupSchema(schemaKey).flatMap {
          case Right(Some(schema)) if isValid(schema) => Sync[F].pure(schema.asRight)
          case Right(Some(schema)) =>
            val error = RepoError(getErrors(schema).toSet, 1, unrecoverable = true).some
            traverseRepos(schemaKey, repos, Map(repo -> error) |+| tried)
          case Right(None) =>
            val error = none[RepoError]
            traverseRepos(schemaKey, repos, Map(repo -> error) |+| tried)
          case Left(e) =>
            val error = RepoError(Set(e), 1, unrecoverable = false).some
            traverseRepos(schemaKey, repos, Map(repo -> error) |+| tried)
        }
    }
  }
}
