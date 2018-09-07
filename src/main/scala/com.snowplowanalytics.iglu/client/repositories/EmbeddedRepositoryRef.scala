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
import com.snowplowanalytics.iglu.client.validation.ProcessingMessage

// Scala
import scala.io.Source

// Cats
import cats.instances.option._
import cats.instances.either._
import cats.syntax.apply._
import cats.syntax.either._
import cats.syntax.option._
import cats.syntax.validated._
import cats.syntax.traverse._
import cats.effect.Sync

// circe
import io.circe.Json
import io.circe.parser.parse
import io.circe.optics.JsonPath._

// Iglu Core
import com.snowplowanalytics.iglu.core.SchemaKey

// This project
import utils.{ValidationExceptions => VE, SchemaKeyUtils}

/**
 * Helpers for constructing an EmbeddedRepository.
 * See below for the definition.
 */
object EmbeddedRepositoryRef {

  /**
   * Sniffs a config JSON to determine if this is
   * an embedded repository ref or not.
   *
   * @param config The configuration JSON to sniff
   * @return true if this is the configuration for
   *         an EmbeddedRepositoryRef, else false
   */
  def isEmbedded(config: Json): Boolean =
    root.connection.embedded.json.getOption(config).isDefined

  /**
   * Constructs an EmbeddedRepositoryRef
   * from a Json.
   *
   * @param config The JSON containing the configuration
   *        for this repository reference
   * @return a configured reference to this embedded
   *         repository
   */
  def parse(config: Json): ValidatedNelType[EmbeddedRepositoryRef] = {
    val conf = RepositoryRefConfig.parse(config)
    val path = extractPath(config)
    (conf, path).mapN { EmbeddedRepositoryRef(_, _) }
  }

  /**
   * Returns the path to this embedded repository.
   *
   * @param config The JSON containing the configuration
   *        for this repository reference
   * @return the path to the embedded repository on
   *         Success, or an error String on Failure
   */
  private def extractPath(config: Json): ValidatedNelType[String] =
    root.connection.embedded.path.string
      .getOption(config)
      .toValidNel(
        ProcessingMessage(s"Could not extract connection.embedded.path from ${config.spaces2}"))
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
   * a json.
   *
   * @param schemaKey The SchemaKey uniquely identifying the schema in Iglu
   * @return either a schema Json on success, or a ProcessingMessage on Failure
   */
  def lookupSchema[F[_]: Sync](schemaKey: SchemaKey): F[Either[ProcessingMessage, Option[Json]]] = {
    val schemaPath = SchemaKeyUtils.toPath(path, schemaKey)
    val streamOpt = Option(getClass.getResource(schemaPath))
      .map(_.openStream())

    Sync[F].delay {
      try {
        streamOpt
          .map(stream => Source.fromInputStream(stream).mkString)
          .traverse(jsonString => parse(jsonString))
          .leftMap(failure =>
            ProcessingMessage(
              s"Problem parsing $schemaPath as JSON in $descriptor Iglu repository ${config.name}: %s"
                .format(VE.stripInstanceEtc(failure.message))))
      } catch {
        case e: Throwable =>
          ProcessingMessage(
            s"Unknown problem reading and parsing ${schemaPath} in ${descriptor} Iglu repository ${config.name}: ${VE
              .getThrowableMessage(e)}").asLeft
      } finally {
        streamOpt.foreach(_.close())
      }
    }
  }

}
