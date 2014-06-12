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

// This project
import validation.ProcessingMessageMethods
import ProcessingMessageMethods._

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
  def apply(ref: JsonNode): ValidatedNel[EmbeddedRepositoryRef] =
    apply(fromJsonNode(ref))

  /**
   * Constructs an EmbeddedRepositoryRef
   * from a JValue.
   *
   * @param config The JSON containing the configuration
   *        for this repository reference
   * @return a configured reference to this embedded
   *         repository
   */
  def apply(config: JValue): ValidatedNel[EmbeddedRepositoryRef] = {
    val conf = RepositoryRefConfig(config)
    val path = extractPath(config)
    (conf.toValidationNel |@| path.toValidationNel) { EmbeddedRepositoryRef(_, _) }
  }

  /**
   * Returns the path to this embedded repository.
   *
   * @param ref The JSON containing the configuration
   *        for this repository reference
   * @return the path to the embedded repository on
   *         Success, or an error String on Failure
   */
  // TODO: implement this properly
  def extractPath(config: JValue): Validated[String] =
    "/iglu-cache".success

}

/**
 * An embedded repository is one which is embedded
 * inside the calling code, e.g. inside the jar's
 * resources folder.
 */
case class EmbeddedRepositoryRef(
  override val config: RepositoryRefConfig,
  path: String) extends RepositoryRef {

  /**
   * Prioritize searching this class of repository because
   * it is low cost.
   */
  override val classPriority: Int = 1

  /**
   * Human-readable descriptor for this
   * type of repository ref.
   */
  val descriptor = "embedded"

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
  // TODO: we should distinguish between not found and
  // invalid JSON
  def lookupSchema(schemaKey: SchemaKey): Validated[Option[JsonNode]] = {
    val schemaPath = s"${path}/schemas/${schemaKey.toPath}"
    try {
      JsonLoader.fromResource(schemaPath).some.success
    } catch {
      case e: Throwable =>
        s"Cannot find schema ${schemaKey} in ${descriptor} Iglu repository ${config.name}".fail.toProcessingMessage
    }
  }
}
