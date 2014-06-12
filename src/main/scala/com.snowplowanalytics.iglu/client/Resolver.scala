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

// Scala
import scala.annotation.tailrec

// Scalaz
import scalaz._
import Scalaz._

// json4s
import org.json4s._
import org.json4s.JsonDSL._
import org.json4s.jackson.JsonMethods._

// This project
import repositories.{
  RepositoryRef,
  EmbeddedRepositoryRef
}
import validation.ValidatableJsonMethods

/**
 * Companion object. Lets us create a Resolver from
 * a JsonNode or JValue.
 */
object Resolver {

  private val ConfigurationSchema = SchemaKey("com.snowplowanalytics.iglu", "resolver-config", "jsonschema", "1-0-0")

  /**
   * Constructs a Resolver instance from an arg array
   * of RepositoryRefs.
   *
   * @param cacheSize The size of the cache
   * @param headRef The first RepositoryRef
   * @param tailRefs Any further RepositoryRefs
   * @return a configured Resolver instance
   */
  def apply(cacheSize: Int, refs: RepositoryRef*): Resolver =
    Resolver(cacheSize, List[RepositoryRef](refs: _*))

  /**
   * Constructs a Resolver instance from a JsonNode.
   *
   * @param config The JSON containing the configuration
   *        for this resolver
   * @return a configured Resolver instance
   */
  def apply(config: JsonNode): Validation[String, Resolver] = {

    // We can use the bootstrap Resolver for working
    // with JSON Schemas here.
    implicit val resolver = Bootstrap.Resolver
    implicit lazy val formats = org.json4s.DefaultFormats

    import ValidatableJsonMethods._

    // Check it passes validation
    config.validateAndIdentifySchema(dataOnly = true) match {
      case Success((key, node)) if key == ConfigurationSchema => {
        val json = fromJsonNode(node)
        val cacheSize = (json \ "cacheSize").extract[Int]

        "TODO".fail
      }
      case Success((key, node)) if key != ConfigurationSchema =>
        s"Expected a ${ConfigurationSchema} as resolver configuration, got: ${key}".fail
      case Failure(err) => {
        System.out.println(err)
        "Resolver configuration failed JSON Schema validation".fail
      }
    }
  }

  /**
   * Creates a Resolver instance from a JValue.
   *
   * @param config The JSON containing the configuration
   *        for this resolver
   * @return a configured Resolver instance
   */
  def apply(config: JValue): Validation[String, Resolver] =
    apply(asJsonNode(config))

  /**
   * Extracts a List of RepositoryRefs from the
   * given JValue.
   *
   * @param repositoriesConfig The JSON containing
   *        all of the repository configurations
   * @return our assembled List of RepositoryRefs
   */
  // TODO: fix the return type
  private[this] def getRepositoryRefs(repositoriesConfig: JValue): Validation[String, RepositoryRefs] = {
    // TODO: implement this
    Nil.success
  }

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
  // TODO: fix the return type
  private[this] def buildRepositoryRef(repositoryConfig: JValue): Validation[String, RepositoryRef] =
    // TODO: implement this
    if (true) {
      EmbeddedRepositoryRef(repositoryConfig)
    } else if (false) {
      EmbeddedRepositoryRef(repositoryConfig)
    } else {
      s"Configuration unrecognizable as either embedded or HTTP repository".fail
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
) extends Lookup with UnsafeLookup {
  
  private[this] val allRepos = Bootstrap.Repo :: repos

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
  // TODO: should we accumulate a Nel on Failure side?
  def lookupSchema(schemaKey: SchemaKey): Validation[String, JsonNode] = {

    @tailrec def recurse(schemaKey: SchemaKey, tried: RepositoryRefs, remaining: RepositoryRefs): Validation[String, JsonNode] = {
      remaining match {
        case Nil => {
          val tr = tried.map(t => s"${t.config.name} [${t.descriptor}]").mkString(", ")
          s"Could not find schema with key ${schemaKey} in any repository, tried: ${tr}".fail
        }
        case repo :: repos => {
          repo.lookupSchema(schemaKey) match {
            case Success(schema) => cache.store(schemaKey, schema).success
            case _               => recurse(schemaKey, tried.::(repo), repos)
          }
        }
      }
    }

    cache.get(schemaKey) match {
      case Some(schema) => schema.success
      case None         => recurse(schemaKey, Nil, prioritizeRepos(schemaKey))
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
  def lookupSchema(schemaUri: String): Validation[String, JsonNode] =
    for {
      k <- SchemaKey(schemaUri)
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
