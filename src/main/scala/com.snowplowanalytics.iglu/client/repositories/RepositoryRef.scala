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
package repositories

// Jackson
import com.fasterxml.jackson.databind.JsonNode

// Scalaz
import scalaz._
import Scalaz._

// json4s
import org.json4s._
import org.json4s.JsonDSL._
import org.json4s.jackson.JsonMethods._

/**
 * Common parent of all RepositoryRef classes.
 *
 * @param vendorPrefixes is the list (possibly
 *        empty) of schema vendors, or prefixes
 *        of schema vendors, to 
 */
abstract class RepositoryRef(val vendorPrefixes: List[String]) {

  /**
   * All JSONs can pass this validation. We return
   * this in the case of OptimisticResolution if we
   * can't find the requested schema.
   */
  private val IdentitySchema = asJsonNode(parse("{}"))

  /**
   * Abstract method. All repositories with a
   * search priority of 1 will be checked before
   * any repository with a search priority of 2.
   *
   * @return the search priority for this class
   *         of repository ref
   */
  def searchPriority: Int

  /**
   * Abstract method. Provide a concrete
   * implementation for how to lookup a schema
   * in this type of repository.
   *
   * @param schemaKey The SchemaKey uniquely identifying
   *        the schema in Iglu
   * @return a Validation boxing either the Schema's
   *         JsonNode on Success, or an error String
   *         on Failure 
   */
  def lookupSchema(schemaKey: SchemaKey): Validation[String, JsonNode]

  /**
   * Abstract method. Provide a concrete
   * implementation for how to lookup a schema
   * in this type of repository in an unsafe fashion.
   *
   * ONLY implement in a sub-class if the resolution has
   * a good chance of succeeding (e.g. no network I/O).
   *
   * @param schemaKey The SchemaKey uniquely identifying
   *        the schema in Iglu
   * @return the JsonNode representing this schema
   */
  def unsafeLookupSchema(schemaKey: SchemaKey): JsonNode

  /**
   * Helper to check if this repository should take
   * priority because of a vendor prefix match. Returns
   * true if 
   *
   * @param schemaKey The SchemaKey uniquely identifying
   *        the schema in Iglu. We will use the vendor
   *        within the SchemaKey to match against our
   *        prefixes
   * @return whether this is a priority lookup or
   *         not
   */
  def vendorMatched(schemaKey: SchemaKey): Boolean = {
    val matches = for {
      p <- vendorPrefixes
      m = schemaKey.vendor.startsWith(p)
    } yield m
    matches.foldLeft(false)(_ || _) // Any match
  }
}
