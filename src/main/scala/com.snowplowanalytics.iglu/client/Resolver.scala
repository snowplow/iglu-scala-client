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
 * Resolves schemas from one or more Iglu schema
 * repositories.
 *
 * This is an extremely primitive implementation.
 * Currently it only supports lookups of schemas
 * specified by the exact same version (i.e.
 * MODEL-REVISION-ADDITION).
 */
case class Resolver(
  val repos: RepositoryRefNel,
  val lruCache: Int = 500) extends Lookup with UnsafeLookup {
  
  // Initialise the cache
  private val lru: MaybeSchemaLruMap = if (lruCache > 0) Some(new SchemaLruMap(lruCache)) else None

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
  def lookupSchema(schemaKey: SchemaKey): ValidatedJsonNode = {

    @tailrec def recurse(schemaKey: SchemaKey, remainingRepos: RepositoryRefs): ValidatedJsonNode = {
      remainingRepos match {
        case Nil           => s"Could not find schema with key ${schemaKey} in any repository".fail
        case repo :: repos => {
          repo.lookupSchema(schemaKey) match {
            case Success(schema) => cache(schemaKey, schema).success
            case _               => recurse(schemaKey, repos)
          }
        }
      }
    }

    getFromCache(schemaKey) match {
      case Some(schema) => schema.success
      case None         => recurse(schemaKey, prioritizeRepos(schemaKey))
    }
  }

  /**
   * xxx
   *
   * ONLY implement in a sub-class if the resolution has
   * a good chance of succeeding (e.g. no network I/O).
   *
   * @param schemaKey The SchemaKey uniquely identifying
   *        the schema in Iglu
   * @return the JsonNode representing this schema
   */
  def unsafeLookupSchema(schemaKey: SchemaKey): JsonNode = asJsonNode(parse("{}"))

  /**
   * Looks up the given schema key in the cache.
   *
   * @param schemaKey The SchemaKey uniquely identifying
   *        the schema in Iglu
   * @return the schema if found as Some JsonNode or None
   *         if not found, or cache is not enabled.
   */
  def getFromCache(schemaKey: SchemaKey): MaybeJsonNode =
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
  def cache(schemaKey: SchemaKey, schema: JsonNode): JsonNode = {
    lru match {
      case Some(c) => c.put(schemaKey, schema)
      case None    => // Do nothing
    }
    schema
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
    repos.toList.sortBy(r =>
      (!r.vendorMatched(schemaKey), r.groupPriority, r.instancePriority)
    )
}
