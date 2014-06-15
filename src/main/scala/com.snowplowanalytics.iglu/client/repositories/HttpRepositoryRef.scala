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
import java.net.{
  URL,
  UnknownHostException,
  MalformedURLException
}

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
import org.json4s.scalaz.JsonScalaz._
import org.json4s._
import org.json4s.JsonDSL._
import org.json4s.jackson.JsonMethods._

// This project
import validation.ProcessingMessageMethods
import ProcessingMessageMethods._
import utils.{ValidationExceptions => VE}

/**
 * Helpers for constructing an HttpRepository.
 * See below for the definition.
 */
object HttpRepositoryRef {

  implicit val formats = DefaultFormats

  /**
   * Sniffs a config JSON to determine if this is
   * an HTTP-based repository ref or not.
   *
   * @param config The configuration JSON to sniff
   * @return true if this is the configuration for
   *         an HttpRepositoryRef, else false
   */
  def isHttp(config: JValue): Boolean =
    (config \ "connection" \ "http").toSome.isDefined

  /**
   * Constructs an EmbeddedRepositoryRef
   * from a JsonNode.
   *
   * @param config The JSON containing the configuration
   *        for this repository reference
   * @return a configured reference to this embedded
   *         repository
   */
  def parse(config: JsonNode): ValidatedNel[HttpRepositoryRef] =
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
  def parse(config: JValue): ValidatedNel[HttpRepositoryRef] = {
    
    val conf = RepositoryRefConfig.parse(config)
    val url  = extractUrl(config)
    (conf |@| url.toValidationNel) { HttpRepositoryRef(_, _) }
  }

  /**
   * Returns the path to this embedded repository.
   *
   * @param ref The JSON containing the configuration
   *        for this repository reference
   * @return the path to the embedded repository on
   *         Success, or an error String on Failure
   */
  private def extractUrl(config: JValue): Validated[URL] =
    try {
      stringToUrl((config \ "connection" \ "http" \ "uri").extract[String])
    } catch {
      case me: MappingException => s"Could not extract connection.http.uri from ${compact(render(config))}".fail.toProcessingMessage
    }

  /**
   * A wrapper around Java's URL.
   *
   * Exceptions thrown by
   * URI.create():
   * 1. NullPointerException
   *    if uri is null
   * 2. IllegalArgumentException
   *    if uri violates RFC 2396
   *
   * @param url The String to
   *        convert to a URL
   * @return a URLobject, or an
   *         error message, all
   *         wrapped in a Validation
   */       
  private def stringToUrl(url: String): Validated[URL] =
    (try {
      (new URL(url)).success
    } catch {
      case npe: NullPointerException => "Provided URL was null".fail
      case mue: MalformedURLException => "Provided URL string [%s] is malformed: [%s]".format(url, mue.getMessage).fail
      case iae: IllegalArgumentException => "Provided URL string [%s] violates RFC 2396: [%s]".format(url, ExceptionUtils.getRootCause(iae).getMessage).fail
      case e: Throwable => "Unexpected error creating URL from string [%s]: [%s]".format(url, e.getMessage).fail
    }).toProcessingMessage

}

/**
 * An HTTP repository is one which is accessible over
 * HTTP.
 */
case class HttpRepositoryRef(
  override val config: RepositoryRefConfig,
  uri: URL) extends RepositoryRef {

  /**
   * De-prioritize searching this class of repository because
   * it is high cost.
   */
  override val classPriority: Int = 100

  /**
   * Human-readable descriptor for this
   * type of repository ref.
   */
  val descriptor = "HTTP"

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
  // TODO: this isn't working when there is a network outage (e.g. running test suite on Tube)
  def lookupSchema(schemaKey: SchemaKey): Validated[Option[JsonNode]] = {
    try {
      for {
        url <- HttpRepositoryRef.stringToUrl(s"${uri.toString}/schemas/${schemaKey.toPath}")
        sch = JsonLoader.fromURL(url).some
      } yield sch
    } catch {
      case jpe: JsonParseException =>
        s"Problem parsing ${schemaKey} as JSON in ${descriptor} Iglu repository ${config.name}: %s".format(VE.stripInstanceEtc(jpe.getMessage)).fail.toProcessingMessage
      case uhe: UnknownHostException =>
        s"Unknown host issue fetching ${schemaKey} in ${descriptor} Iglu repository ${config.name}: ${uhe.getMessage}".fail.toProcessingMessage
      case e: Throwable => None.success
    }
  }
}
