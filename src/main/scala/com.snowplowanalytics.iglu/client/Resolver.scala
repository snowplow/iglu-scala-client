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

// Scalaz
import scalaz._
import Scalaz._

// json4s
import org.json4s._
import org.json4s.JsonDSL._
import org.json4s.jackson.JsonMethods._

// This project
import repositories.RepositoryRef

/** How to handle schema resolution */
sealed trait ResolutionMode
/** Return failure if schema not found */
object PessimisticResolution extends ResolutionMode
/** Return identity schema if schema not found */
object OptimisticResolution extends ResolutionMode

object Resolver {

  /**
   * All JSONs can pass this validation. We return
   * this in the case of OptimisticResolution if we
   * can't find the requested schema.
   */
  private val IdentitySchema = asJsonNode(parse("{}"))
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
class Resolver(
  repos: RepositoryRefNel,
  mode: ResolutionMode,
  lruCache: Int = 500) extends Lookup with UnsafeLookup {
  
  // Initialise the cache
  private val lru: MaybeSchemaLruMap = if (lruCache > 0) Some(new SchemaLruMap(lruCache)) else None

  /**
   * xxx
   *
   * @param schemaKey The SchemaKey uniquely identifying
   *        the schema in Iglu
   * @return a Validation boxing either the Schema's
   *         JsonNode on Success, or an error String
   *         on Failure 
   */
  def lookupSchema(schemaKey: SchemaKey): ValidatedJsonNode = "oh no".fail

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
  def unsafeLookupSchema(schemaKey: SchemaKey): JsonNode = Resolver.IdentitySchema

  /**
   * Re-sorts our Nel of RepositoryRefs into the
   * optimal order for querying.
   *
   * @param schemaKey SchemaKey uniquely identifying
   *        the schema in Iglu
   * @return the prioritized Nel of RepositoryRefs.
   *         Pragmatically sorted to minimize lookups.
   */
  def prioritizeRepos(schemaKey: SchemaKey): RepositoryRefNel =
    repos.toList.sortBy(r =>
      (r.vendorMatched(schemaKey), r.groupPriority, r.instancePriority)
    ) match {
      case x :: xs => NonEmptyList[RepositoryRef](x, xs: _*)
      case Nil     => throw new RuntimeException("List to Nel to List round-trip failed. Should never happen")
    }
}
