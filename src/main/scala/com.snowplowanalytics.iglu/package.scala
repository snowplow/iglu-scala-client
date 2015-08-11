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

// JSON Schema Validator
import com.github.fge.jsonschema.core.report.ProcessingMessage

// Scalaz
import scalaz._
import Scalaz._

// LRU
import com.twitter.util.LruMap

// This project
import client.SchemaKey
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
   * Helpful archetypes
   */
  type Validated[A] = Validation[ProcessingMessage, A]
  type ValidatedNel[A] = ValidationNel[ProcessingMessage, A]

  /**
   * Our LRU cache of schemas
   */
  type SchemaLruMap = LruMap[SchemaKey, ValidatedNel[JsonNode]]

  /**
   * Our List (possibly empty) of Iglu repositories
   */
  type RepositoryRefs = List[RepositoryRef]

  /**
   * Our list (possibly empty) of Processing Messages
   */
  type ProcessingMessages = List[ProcessingMessage]

  /**
   * Our non-empty list of Processing Messages
   */
  type ProcessingMessageNel = NonEmptyList[ProcessingMessage]

  /**
   * Type alias for a SchemaVer-based version.
   *
   * TODO: update this in the future to be
   * a full-fledged case class or similar:
   * https://github.com/snowplow/iglu-scala-client/issues/20
   */
  type SchemaVer = String

  /**
   * Type alias for a Tuple2
   * containing a SchemaKey and its
   * corresponding JsonNode.
   */
  type JsonSchemaPair = Tuple2[SchemaKey, JsonNode]
}
