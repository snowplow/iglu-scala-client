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
import com.fasterxml.jackson.databind.ObjectMapper
import com.snowplowanalytics.iglu.client.validation.ProcessingMessage

// Jackson
import com.fasterxml.jackson.core.JsonParseException
import com.fasterxml.jackson.databind.JsonNode
import com.github.fge.jackson.JsonLoader

// Cats
import cats.instances.option._
import cats.instances.either._
import cats.syntax.apply._
import cats.syntax.validated._
import cats.syntax.either._
import cats.syntax.traverse._

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

  implicit val formats = DefaultFormats

  /**
   * Sniffs a config JSON to determine if this is
   * an embedded repository ref or not.
   *
   * @param config The configuration JSON to sniff
   * @return true if this is the configuration for
   *         an EmbeddedRepositoryRef, else false
   */
  def isEmbedded(config: JValue): Boolean =
    (config \ "connection" \ "embedded").toSome.isDefined

  /**
   * Constructs an EmbeddedRepositoryRef
   * from a JsonNode.
   *
   * @param config The JSON containing the configuration
   *        for this repository reference
   * @return a configured reference to this embedded
   *         repository
   */
  def parse(config: JsonNode): ValidatedNelType[EmbeddedRepositoryRef] =
    parse(fromJsonNode(config))

  /**
   * Constructs an EmbeddedRepositoryRef
   * from a JValue.
   *
   * @param config The JSON containing the configuration
   *        for this repository reference
   * @return a configured reference to this embedded
   *         repository
   */
  def parse(config: JValue): ValidatedNelType[EmbeddedRepositoryRef] = {
    val conf = RepositoryRefConfig.parse(config)
    val path = extractPath(config)
    (conf, path.toValidatedNel).mapN { EmbeddedRepositoryRef(_, _) }
  }

  /**
   * Returns the path to this embedded repository.
   *
   * @param ref The JSON containing the configuration
   *        for this repository reference
   * @return the path to the embedded repository on
   *         Success, or an error String on Failure
   */
  private def extractPath(config: JValue): ValidatedType[String] =
    try {
      (config \ "connection" \ "embedded" \ "path").extract[String].valid
    } catch {
      case me: MappingException =>
        s"Could not extract connection.embedded.path from ${compact(render(config))}".invalid.toProcessingMessage
    }

}

/**
 * An embedded repository is one which is embedded
 * inside the calling code, e.g. inside the jar's
 * resources folder.
 */
case class EmbeddedRepositoryRef(override val config: RepositoryRefConfig, path: String)
    extends RepositoryRef {

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
  // TODO: would be nice to abstract out failure.toProcessingMessage, and scrubbing
  def lookupSchema(schemaKey: SchemaKey): ValidatedType[Option[JsonNode]] = {
    val schemaPath = s"${path}/schemas/${schemaKey.toPath}"
    val streamOpt =
      Option(EmbeddedRepositoryRef.getClass.getResource(schemaPath)).map(_.openStream())

    try {
      streamOpt
        .traverse(stream => new ObjectMapper().readTree(stream).asRight[ProcessingMessage])
        .toValidated
    } catch {
      case jpe: JsonParseException => // Child of IOException so match first
        s"Problem parsing ${schemaPath} as JSON in ${descriptor} Iglu repository ${config.name}: %s"
          .format(VE.stripInstanceEtc(jpe.getMessage))
          .invalid
          .toProcessingMessage
      case ioe: IOException =>
        None.valid // Schema not found
      case e: Throwable =>
        s"Unknown problem reading and parsing ${schemaPath} in ${descriptor} Iglu repository ${config.name}: ${VE
          .getThrowableMessage(e)}".invalid.toProcessingMessage
    } finally {
      streamOpt.foreach(_.close())
    }
  }
}
