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
package com.snowplowanalytics.iglu

// Jackson
import com.fasterxml.jackson.databind.JsonNode

// Scalaz
import scalaz._
import Scalaz._

// LRU
import com.twitter.util.LruMap

// This project
import client.repositories.RepositoryRef

/**
 * Scala package object to hold types,
 * helper methods etc.
 *
 * See:
 * http://www.artima.com/scalazine/articles/package_objects.html
 */
package object client {

  /**
   * Type alias for a Validation
   * containing a String for Failure
   * or any type of Success.
   *
   * @tparam A the type of Success
   */
  type Validated[A] = Validation[String, A]

  /**
   * An Option-boxed JsonNode
   */
  type MaybeJsonNode = Option[JsonNode]

  /**
   * Our LRU cache of schemas
   */
  type SchemaLruMap = LruMap[SchemaKey, JsonNode]

  /**
   * Option-wrapped cache
   */
  type MaybeSchemaLruMap = Option[SchemaLruMap]

  /**
   * Our Nel of resolution repositories
   */
  type RepositoryRefNel = NonEmptyList[RepositoryRef]

  /**
   * Our List (possibly empty) of resolution repositories
   */
  type RepositoryRefs = List[RepositoryRef]

  /**
   * A Validation-boxed JsonNode
   */
  type ValidatedJsonNode = Validated[JsonNode]

  /**
   * Type alias for a SchemaVer-based version.
   *
   * We may update this in the future to be
   * a full-fledged case class or similar.
   */
  type SchemaVer = String
}
