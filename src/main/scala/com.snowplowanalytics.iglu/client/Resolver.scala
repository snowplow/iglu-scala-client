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
import repositories.RepositoryRef

/**
 * Companion object. Lets us create a Resolver from
 * a JsonNode or JValue.
 */
object Resolver {

  /**
   * Constructs a Resolver instance from an arg array
   * of RepositoryRefs.
   *
   * @param cacheSize The size of the cache
   * @param headRef The first RepositoryRef
   * @param tailRefs Any further RepositoryRefs
   * @return a configured Resolver instance
   */
  def apply(cacheSize: Int, headRef: RepositoryRef, tailRefs: RepositoryRef*): Resolver =
    Resolver(cacheSize, NonEmptyList[RepositoryRef](headRef, tailRefs: _*))

  /**
   * Constructs a Resolver instance from a JsonNode.
   *
   * @param config The JSON containing the configuration
   *        for this resolver
   * @return a configured Resolver instance
   */
  def apply(config: JsonNode): Validated[Resolver] = {

    // We can use the bootstrap Resolver for working
    // with JSON Schemas here.
    implicit val resolver = Bootstrap.Resolver

    // Check it passes validation
    // TODO

    // Now retrieve cache size
    // TODO

    // Now let's loop through and create our RepositoryRefs
    // TODO

    "TODO".fail
  }

  /**
   * Creates a Resolver instance from a JValue.
   *
   * @param config The JSON containing the configuration
   *        for this resolver
   * @return a configured Resolver instance
   */
  def apply(config: JValue): Validated[Resolver] =
    apply(asJsonNode(config))

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
  repos: RepositoryRefNel
) extends Lookup with UnsafeLookup {
  
  private[this] val allRepos = Bootstrap.Repo :: repos.toList

  /**
   * Our LRU cache.
   */
  object cache {

    private val lru: MaybeSchemaLruMap = if (cacheSize > 0) Some(new SchemaLruMap(cacheSize)) else None

    /**
     * Looks up the given schema key in the cache.
     *
     * @param schemaKey The SchemaKey uniquely identifying
     *        the schema in Iglu
     * @return the schema if found as Some JsonNode or None
     *         if not found, or cache is not enabled.
     */
    def get(schemaKey: SchemaKey): MaybeJsonNode =
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
  def lookupSchema(schemaKey: SchemaKey): ValidatedJsonNode = {

    @tailrec def recurse(schemaKey: SchemaKey, remainingRepos: RepositoryRefs): ValidatedJsonNode = {
      remainingRepos match {
        case Nil           => s"Could not find schema with key ${schemaKey} in any repository".fail
        case repo :: repos => {
          repo.lookupSchema(schemaKey) match {
            case Success(schema) => cache.store(schemaKey, schema).success
            case _               => recurse(schemaKey, repos)
          }
        }
      }
    }

    cache.get(schemaKey) match {
      case Some(schema) => schema.success
      case None         => recurse(schemaKey, prioritizeRepos(schemaKey))
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
  def lookupSchema(schemaUri: String): ValidatedJsonNode =
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
