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
import com.github.fge.jackson.JsonLoader
import com.fasterxml.jackson.databind.JsonNode
import com.fasterxml.jackson.databind.node.ObjectNode

// json4s
import org.json4s._
import org.json4s.JsonDSL._
import org.json4s.jackson.JsonMethods._

// LRU
import com.twitter.util.LruMap

// Scalaz
import scalaz._
import Scalaz._

// This project
import repositories.RepositoryRef

/**
 * Resolves schemas from one or more Iglu schema
 * repositories.
 *
 * This is an extremely primitive implementation.
 * Currently it only supports access to locally
 * stored schemas specified by the exact same
 * version (i.e. MODEL-REVISION-ADDITION).
 */
class Resolver(repos: NonEmptyList[RepositoryRef], mode: ResolutionMode, lruCache: Int = 500) {
  
  // Initialise the cache
  private val lru =
    if (lruCache > 0) {
      new LruMap[SchemaKey, JsonNode](lruCache)
    } else {
      throw new IllegalArgumentException("LRU cache size less than 1 (${lruCache}) makes no sense")
    }

  // TODO: this should become a set of resolvers.
  private val localPath = "/iglu-cache"

  // All JSONs can pass this validation
  private val identitySchema = asJsonNode(parse("{}"))

  // TODO: add in a mutable cache of JSON Schemas to prevent
  // a) lots of JsonNode instantiation and (later) b) lots
  // of unnecessary HTTP requests

  /**
   * Retrieves an IgluSchema from the Iglu Repo as
   * a JsonNode.
   *
   * @param schemaKey The SchemaKey uniquely identifies
   *        the schema in Iglu
   * @return a Validation boxing either the Schema's
   *         JsonNode on Success, or an error String
   *         on Failure 
   */
  def lookupSchema(schemaKey: SchemaKey): Validation[String, JsonNode] = {
    try {
      unsafeLookupSchema(schemaKey).success
    } catch {
      case e: Throwable =>
        if (mode == OptimisticResolution) {
          identitySchema.success
        } else {
          s"Cannot find schema ${schemaKey} in any Iglu repository".fail
        }
    }
  }

  /**
   * Retrieves an IgluSchema from the Iglu Repo as
   * a JsonNode. Convenience function which converts
   * an Iglu-format schema URI to a SchemaKey to
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
   * Retrieves an IgluSchema from the Iglu Repo as
   * a JsonNode. Unsafe - only use when you know the
   * schema is available locally.
   *
   * @param schemaKey The SchemaKey uniquely identifies
   *        the schema in Iglu
   * @return the JsonNode representing this schema
   */
  def unsafeLookupSchema(schemaKey: SchemaKey): JsonNode = {
    val schemaPath = s"${localPath}/schemas/${schemaKey.toPath}"
    JsonLoader.fromResource(schemaPath)
  }
}
