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

/**
 * Common parent of all RepositoryRef classes.
 *
 * @param refPriority The priority of this
 *        reference instance
 * @param vendorPrefixes The list (possibly
 *        empty) of schema vendors, or prefixes
 *        of schema vendors, to 
 */
abstract class RepositoryRef(
  val refPriority: Int,
  val vendorPrefixes: List[String]) extends Lookup {

  /**
   * Abstract method. All repositories with a
   * search priority of 1 will be checked before
   * any repository with a search priority of 2.
   *
   * @return the search priority for this class
   *         of repository ref
   */
  def priority: Int

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
