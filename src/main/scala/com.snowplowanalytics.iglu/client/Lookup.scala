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
// TODO: remove this once we have a type alias in play for lookupSchema's retval
import scalaz._
import Scalaz._

/**
 * Trait for a schema lookup.
 */
trait Lookup {

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
   * @param schemaKey The SchemaKey uniquely identifying
   *        the schema in Iglu
   * @return the JsonNode representing this schema
   */
  def unsafeLookupSchema(schemaKey: SchemaKey): JsonNode
}
