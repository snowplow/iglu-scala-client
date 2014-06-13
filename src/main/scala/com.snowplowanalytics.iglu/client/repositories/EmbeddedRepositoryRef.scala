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

// Java
import java.io.IOException

// Apache Commons
import org.apache.commons.lang3.exception.ExceptionUtils

// Jackson
import com.fasterxml.jackson.core.JsonParseException
import com.fasterxml.jackson.databind.JsonNode
import com.github.fge.jackson.JsonLoader

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
import utils.{ValidationExceptions => VE}

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
  def parse(ref: JsonNode): ValidatedNel[EmbeddedRepositoryRef] =
    parse(fromJsonNode(ref))

  /**
   * Constructs an EmbeddedRepositoryRef
   * from a JValue.
   *
   * @param config The JSON containing the configuration
   *        for this repository reference
   * @return a configured reference to this embedded
   *         repository
   */
  def parse(config: JValue): ValidatedNel[EmbeddedRepositoryRef] = {
    val conf = RepositoryRefConfig.parse(config)
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
  private def extractPath(config: JValue): Validated[String] =
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
  // TODO: would be nice to abstract out fail.toProcessingMessage, and scrubbing
  def lookupSchema(schemaKey: SchemaKey): Validated[Option[JsonNode]] = {
    val schemaPath = s"${path}/schemas/${schemaKey.toPath}"
    try {
      JsonLoader.fromResource(schemaPath).some.success
    } catch {
      case jpe: JsonParseException => // Child of IOException so match first
        s"Problem parsing ${schemaPath} as JSON in ${descriptor} Iglu repository ${config.name}: %s".format(VE.stripInstanceEtc(jpe.getMessage)).fail.toProcessingMessage
      case ioe: IOException =>
        None.success // Schema not found
      case e: Throwable =>
        s"Unknown problem reading and parsing ${schemaPath} in ${descriptor} Iglu repository ${config.name}: %s".format(ExceptionUtils.getRootCause(e).getMessage).fail.toProcessingMessage
    }
  }
}
