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

// JSON Schema
import com.github.fge.jsonschema.core.report.{
  ProcessingMessage,
  LogLevel
}

// Scala
import scala.annotation.tailrec

// Scalaz
import scalaz._
import Scalaz._

// json4s
import org.json4s.scalaz.JsonScalaz._
import org.json4s._
import org.json4s.JsonDSL._
import org.json4s.jackson.JsonMethods._

// This project
import repositories.{
  RepositoryRef,
  EmbeddedRepositoryRef,
  HttpRepositoryRef
}
import validation.ValidatableJsonMethods
import validation.ProcessingMessageMethods
import ProcessingMessageMethods._

/**
 * Companion object. Lets us create a Resolver from
 * a JsonNode or JValue.
 */
object Resolver {

  private val ConfigurationSchema = SchemaCriterion("com.snowplowanalytics.iglu", "resolver-config", "jsonschema", 1, Some(0), Some(0))

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
  def parse(config: JValue): ValidatedNel[Resolver] =
    parse(asJsonNode(config))

  /**
   * Constructs a Resolver instance from a JsonNode.
   *
   * @param config The JSON containing the configuration
   *        for this resolver
   * @return a configured Resolver instance
   */
  def parse(config: JsonNode): ValidatedNel[Resolver] = {

    // We can use the bootstrap Resolver for working
    // with JSON Schemas here.
    implicit val resolver = Bootstrap.Resolver
    implicit lazy val formats = org.json4s.DefaultFormats

    import ValidatableJsonMethods._

    // Check it passes validation
    config.verifySchemaAndValidate(ConfigurationSchema, dataOnly = true) match {
      case Success(node) => {

        val json = fromJsonNode(node)
        val cacheSize = field[Int]("cacheSize")(json).leftMap(_.map(_.toString.toProcessingMessage))
        val repositoryRefs: ValidatedNel[RepositoryRefs] = (field[List[JValue]]("repositories")(json)).fold(
          f => f.map(_.toString.toProcessingMessage).fail,
          s => getRepositoryRefs(s)
        )
        (cacheSize |@| repositoryRefs) {
          Resolver(_, _)
        }
      }
      case Failure(err) =>
        (err.<::("Resolver configuration failed JSON Schema validation".toProcessingMessage)).fail[Resolver]
    }
  }

  /**
   * Extracts a List of RepositoryRefs from the
   * given JValue.
   *
   * @param repositoriesConfig The JSON containing
   *        all of the repository configurations
   * @return our assembled List of RepositoryRefs
   */
  private[client] def getRepositoryRefs(repositoryConfigs: List[JValue]): ValidatedNel[RepositoryRefs] =
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
  private[client] def buildRepositoryRef(repositoryConfig: JValue): ValidatedNel[RepositoryRef] = {
    val rc = repositoryConfig
    if (EmbeddedRepositoryRef.isEmbedded(rc)) {
      EmbeddedRepositoryRef.parse(rc)
    } else if (HttpRepositoryRef.isHttp(rc)) {
      HttpRepositoryRef.parse(rc)
    } else {
      s"Configuration unrecognizable as either embedded or HTTP repository".fail.toProcessingMessageNel
    }
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
case class Resolver(
  cacheSize: Int = 500,
  repos: RepositoryRefs
) {
  
  private[client] val allRepos = Bootstrap.Repo :: repos

  /**
   * Our LRU cache.
   */
  object cache {

    private val lru: Option[SchemaLruMap] = if (cacheSize > 0) Some(new SchemaLruMap(cacheSize)) else None

    /**
     * Looks up the given schema key in the cache.
     *
     * @param schemaKey The SchemaKey uniquely identifying
     *        the schema in Iglu
     * @return the schema if found as Some JsonNode or None
     *         if not found, or cache is not enabled.
     */
    def get(schemaKey: SchemaKey): Option[JsonNode] =
      for {
        l <- lru
        k <- l.get(schemaKey)
      } yield k

    /**
     * Caches and returns the given schema. Does
     * nothing if we don't have an LRU cache
     * available.
     *
     * @param schema The provided schema
     * @return the same schema
     */
    def store(schemaKey: SchemaKey, schema: JsonNode): JsonNode = {
      for (l <- lru) {
        l.put(schemaKey, schema)
      }
      schema
    }
  }

  /**
   * Tries to find the given schema in any of the
   * provided repository refs.
   *
   * @param schemaKey The SchemaKey uniquely identifying
   *        the schema in Iglu
   * @return a Validation boxing either the Schema's
   *         JsonNode on Success, or an error String
   *         on Failure 
   */
  def lookupSchema(schemaKey: SchemaKey): ValidatedNel[JsonNode] = {

  /**
   * Tail-recursive function to find our schema in one
   * of our repositories.
   *
   * @param schemaKey The SchemaKey uniquely identifying
   *        this schema in Iglu
   * @param errors A List of error messages collected so
   *        far
   * @param tried A List of repositories we have looked
   *        in fruitlessly so far
   * @param remaining A List of repositories we still
   *        have to look in
   * @return either a Success-boxed schema (as a JsonNode),
   *         or a Failure-boxing Nel of ProcessingMessages
   */
    @tailrec def recurse(schemaKey: SchemaKey, errors: ProcessingMessages, tried: RepositoryRefs, remaining: RepositoryRefs): ValidatedNel[JsonNode] = {
      remaining match {
        case Nil =>
          collectErrors(schemaKey, errors, tried).fail
        case repo :: repos => {
          repo.lookupSchema(schemaKey) match {
            case Success(Some(schema)) => cache.store(schemaKey, schema).success
            case Success(None)         => recurse(schemaKey, errors, tried.::(repo), repos)
            case Failure(e)            => recurse(schemaKey, errors.::(e), tried.::(repo), repos)
          }
        }
      }
    }

    cache.get(schemaKey) match {
      case Some(schema) => schema.success
      case None         => recurse(schemaKey, Nil, Nil, prioritizeRepos(schemaKey))
    }
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
  def lookupSchema(schemaUri: String): ValidatedNel[JsonNode] =
    for {
      k <- SchemaKey.parseNel(schemaUri)
      s <- lookupSchema(k)
    } yield s

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
      case Success(schema) => schema
      case Failure(err)    => throw new RuntimeException(s"Unsafe schema lookup failed: ${err}")
    }

  /**
   * Collects together the errors for a failed
   * lookup into a NonEmptyList.
   *
   * @param schemaKey The SchemaKey uniquely identifying
   *        this schema in Iglu
   * @param errors A (possibly empty) List of error
   *        messages
   * @param tried A (never empty) List of repositories
   *        we looked up our SchemaKey in
   * @return a NonEmptyList of ProcessingMessages
   */
  // TODO: tried really ought to be a Nel
  private[client] def collectErrors(schemaKey: SchemaKey, errors: ProcessingMessages, tried: RepositoryRefs): ProcessingMessageNel = {

    val repos = tried.map(t => s"${t.config.name} [${t.descriptor}]")
    val notFound = new ProcessingMessage()
                     .setLogLevel(LogLevel.ERROR)
                     .setMessage(s"Could not find schema with key ${schemaKey} in any repository, tried:")
                     .put("repositories", asJsonNode(repos))

    NonEmptyList[ProcessingMessage](notFound, errors: _*)
  }

  /**
   * Re-sorts our Nel of RepositoryRefs into the
   * optimal order for querying (optimal =
   * minimizing unsafe I/O).
   *
   * @param schemaKey SchemaKey uniquely identifying
   *        the schema in Iglu
   * @return the prioritized List of RepositoryRefs.
   *         Pragmatically sorted to minimize lookups.
   */
  private[client] def prioritizeRepos(schemaKey: SchemaKey): RepositoryRefs =
    allRepos.sortBy(r =>
      (!r.vendorMatched(schemaKey), r.classPriority, r.config.instancePriority)
    )
}
