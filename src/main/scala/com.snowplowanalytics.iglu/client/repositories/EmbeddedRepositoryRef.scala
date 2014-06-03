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
import com.github.fge.jackson.JsonLoader
import com.fasterxml.jackson.databind.JsonNode

// Scalaz
import scalaz._
import Scalaz._

// json4s
import org.json4s._
import org.json4s.JsonDSL._
import org.json4s.jackson.JsonMethods._

/**
 * Helpers for constructing an EmbeddedRepository.
 * See below for the definition.
 */
object EmbeddedRepositoryRef {

  /**
   * Constructs an EmbeddedRepositoryRef
   * from a JsonNode.
   *
   * @param ref The JSON containing the configuration
   *        for this repository reference
   * @return a configured reference to this embedded
   *         repository
   */
  def apply(ref: JsonNode): EmbeddedRepositoryRef =
    apply(fromJsonNode(ref))

  /**
   * Constructs an EmbeddedRepositoryRef
   * from a JValue.
   *
   * @param ref The JSON containing the configuration
   *        for this repository reference
   * @return a configured reference to this embedded
   *         repository
   */
  // TODO: implement this properly
  def apply(ref: JValue): EmbeddedRepositoryRef =
    EmbeddedRepositoryRef(Nil, "/iglu-cache")

}

/**
 * An embedded repository is one which is embedded
 * inside the calling code, e.g. inside the jar's
 * resources folder.
 */
case class EmbeddedRepositoryRef(
  override val vendorPrefixes: List[String],
  val resourcePath: String) extends RepositoryRef(vendorPrefixes) {

  /**
   * Prioritize searching this repository because
   * it is low cost.
   */
  def searchPriority: Int = 1

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
    val schemaPath = s"${resourcePath}/schemas/${schemaKey.toPath}"
    JsonLoader.fromResource(schemaPath)
  }

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
        //if (mode == OptimisticResolution) {
        //  identitySchema.success
       // } else {
          s"Cannot find schema ${schemaKey} in any Iglu repository".fail
      //  }
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
   *
  def lookupSchema(schemaUri: String): Validation[String, JsonNode] =
    for {
      k <- SchemaKey(schemaUri)
      s <- lookupSchema(k)
    } yield s */
}
