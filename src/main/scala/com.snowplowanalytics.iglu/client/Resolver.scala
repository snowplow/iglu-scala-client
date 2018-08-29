/*
 * Copyright (c) 2014 Snowplow Analytics Ltd. All rights reserved.
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

// Jackson
import com.fasterxml.jackson.databind.JsonNode
import com.snowplowanalytics.lrumap.LruMap

// JSON Schema

// Scala
import scala.annotation.tailrec

// Cats
import cats.Semigroup
import cats.instances.list._
import cats.instances.set._
import cats.instances.map._
import cats.instances.option._
import cats.syntax.apply._
import cats.syntax.applicativeError._
import cats.syntax.semigroup._
import cats.syntax.option._
import cats.syntax.traverse._
import cats.syntax.validated._
import cats.data.NonEmptyList
import cats.data.Validated._
import cats.effect.IO

// json4s
import org.json4s._
import org.json4s.JsonDSL._
import org.json4s.jackson.JsonMethods._

// This project
import repositories.{EmbeddedRepositoryRef, HttpRepositoryRef, RepositoryRef}
import validation.SchemaValidation.{getErrors, isValid}
import validation.ValidatableJsonMethods
import utils.TemporaryJson4sCatsUtils._
import validation.ProcessingMessage
import validation.ProcessingMessageMethods._

/**
 * Companion object. Lets us create a Resolver from
 * a JsonNode or JValue.
 */
object Resolver {

  private val ConfigurationSchema =
    SchemaCriterion("com.snowplowanalytics.iglu", "resolver-config", "jsonschema", 1, 0, 2)

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
  def apply(cacheSize: Int, refs: RepositoryRef*): Resolver =
    Resolver(cacheSize, List[RepositoryRef](refs: _*))

  /**
   * Creates a Resolver instance from a JValue.
   *
   * @param config The JSON containing the configuration
   *        for this resolver
   * @return a configured Resolver instance
   */
  def parse(config: JValue): ValidatedNelType[Resolver] =
    parse(asJsonNode(config))

  /**
   * Constructs a Resolver instance from a JsonNode.
   *
   * @param config The JSON containing the configuration
   *        for this resolver
   * @return a configured Resolver instance
   */
  def parse(config: JsonNode): ValidatedNelType[Resolver] = {

    // We can use the bootstrap Resolver for working
    // with JSON Schemas here.
    implicit val resolver     = Bootstrap.Resolver
    implicit lazy val formats = org.json4s.DefaultFormats

    import ValidatableJsonMethods._

    // Check it passes validation
    config
      .verifySchemaAndValidate(ConfigurationSchema, dataOnly = true)
      .andThen { node =>
        val json = fromJsonNode(node)
        val cacheSize =
          validatedField[Int]("cacheSize")(json).leftMap(_.map(_.toString.toProcessingMessage))
        val cacheTtl =
          validatedOptionalField[Int]("cacheTtl")(json)
            .leftMap(_.map(_.toString.toProcessingMessage))
        val repositoryRefs: ValidatedNelType[RepositoryRefs] =
          (validatedField[List[JValue]]("repositories")(json)).fold(
            f => f.map(_.toString.toProcessingMessage).invalid,
            s => getRepositoryRefs(s)
          )
        (cacheSize, repositoryRefs, cacheTtl).mapN {
          Resolver(_, _, _)
        }
      }
      .leftMap(errors =>
        errors.prepend("Resolver configuration failed JSON Schema validation".toProcessingMessage))
  }

  /**
   * Extracts a List of RepositoryRefs from the
   * given JValue.
   *
   * @param repositoryConfigs The JSON containing
   *        all of the repository configurations
   * @return our assembled List of RepositoryRefs
   */
  private[client] def getRepositoryRefs(
    repositoryConfigs: List[JValue]): ValidatedNelType[RepositoryRefs] =
    repositoryConfigs.map { conf =>
      buildRepositoryRef(conf)
    }.sequence

  /**
   * Builds a RepositoryRef sub-type from the
   * given JValue. Uses the connection property
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
    repositoryConfig: JValue): ValidatedNelType[RepositoryRef] = {
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
   * Tail-recursive function to find our schema in one
   * of our repositories.
   *
   * @param schemaKey The SchemaKey uniquely identifying
   *                  this schema in Iglu
   * @param remaining A List of repositories we have to look in
   *                  (not-tried yet or with non-404 error)
   * @param tried A Map of repositories with their accumulated errors
   *              we have looked in fruitlessly so far
   * @return either a Success-boxed schema (as a JsonNode),
   *         or a Failure-boxing of Map of repositories with all their
   *         accumulated errors
   */
  @tailrec def traverseRepos(
    schemaKey: SchemaKey,
    remaining: RepositoryRefs,
    tried: RepoFailuresMap): SchemaLookup = {
    remaining match {
      case Nil => tried.invalid
      case repo :: repos => {
        repo.lookupSchema(schemaKey) match {
          case Valid(Some(schema)) if isValid(schema) => schema.valid
          case Valid(Some(schema)) =>
            val error = RepoError(getErrors(schema).toSet, 1, unrecoverable = true).some
            traverseRepos(schemaKey, repos, Map(repo -> error) |+| tried)
          case Valid(None) =>
            val error = none[RepoError]
            traverseRepos(schemaKey, repos, Map(repo -> error) |+| tried)
          case Invalid(e) =>
            val error = RepoError(Set(e), 1, unrecoverable = false).some
            traverseRepos(schemaKey, repos, Map(repo -> error) |+| tried)
        }
      }
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
      message = s"Could not find schema with key $schemaKey in any repository, tried:",
      repositories = Some(asJsonNode(repos))
    )

    NonEmptyList(notFound, failures)
  }

  /**
   * Get Unix epoch timestamp in seconds
   */
  private def currentSeconds: Int =
    (System.currentTimeMillis() / 1000).toInt
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
case class Resolver(
  cacheSize: Int = 500,
  repos: RepositoryRefs,
  cacheTtl: Option[Int] = None
) {
  import Resolver._

  private[client] val allRepos = Bootstrap.Repo :: repos

  /**
   * Our LRU cache.
   */
  object cache {

    private val lru: Option[SchemaLruMap] =
      if (cacheSize > 0)
        Some(LruMap.create[IO, SchemaKey, SchemaLookupStamped](cacheSize).unsafeRunSync())
      else None

    /**
     * Looks up the given schema key in the cache.
     *
     * @param schemaKey The SchemaKey uniquely identifying
     *        the schema in Iglu
     * @return the schema if found as Some JsonNode or None
     *         if not found, or cache is not enabled.
     */
    def get(schemaKey: SchemaKey): Option[SchemaLookup] =
      for {
        l      <- lru
        (t, k) <- l.get(schemaKey).unsafeRunSync()
        if isViable(t)
      } yield k

    /**
     * Caches and returns the given schema. Does
     * nothing if we don't have an LRU cache
     * available.
     *
     * @param schema The provided schema
     * @return the same schema
     */
    def store(schemaKey: SchemaKey, schema: SchemaLookup): SchemaLookup = {
      for (l <- lru) {
        l.put(schemaKey, (currentSeconds, schema)).unsafeRunSync()
      }
      schema
    }

    /**
     * Check if cached value is still valid based on resolver's `cacheTtl`
     *
     * @param storedTstamp timestamp when value was saved
     * @return true if
     */
    private def isViable(storedTstamp: Int): Boolean = {
      cacheTtl match {
        case None => true
        case Some(ttl) =>
          if (currentSeconds - storedTstamp < ttl) true
          else false
      }
    }
  }

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
   *         JsonNode on Success, or an error String
   *         on Failure
   */
  def lookupSchema(schemaKey: SchemaKey, attempts: Int = 3): ValidatedNelType[JsonNode] = {
    val result = cache.get(schemaKey) match {
      case Some(schema) =>
        schema.handleErrorWith { serverErrors =>
          val reposForRetry = getReposForRetry(serverErrors, attempts)
          traverseRepos(schemaKey, Resolver.prioritizeRepos(schemaKey, reposForRetry), serverErrors)
        }
      case None => traverseRepos(schemaKey, prioritizeRepos(schemaKey, allRepos), Map())
    }

    cache.store(schemaKey, result).leftMap(collectErrors(schemaKey, _))
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
   *         JsonNode on Success, or an error String
   *         on Failure
   */
  def lookupSchema(schemaUri: String): ValidatedNelType[JsonNode] =
    SchemaKey
      .parseNel(schemaUri)
      .andThen(k => lookupSchema(k))

  /**
   * Tries to find the given schema in any of the
   * provided repository refs.
   *
   * Unsafe as will throw an exception if the
   * schema cannot be found.
   *
   * @param schemaKey The SchemaKey uniquely identifying
   *        the schema in Iglu
   * @return the JsonNode representing this schema
   */
  def unsafeLookupSchema(schemaKey: SchemaKey): JsonNode =
    lookupSchema(schemaKey) match {
      case Valid(schema) => schema
      case Invalid(err)  => throw new RuntimeException(s"Unsafe schema lookup failed: $err")
    }
}
