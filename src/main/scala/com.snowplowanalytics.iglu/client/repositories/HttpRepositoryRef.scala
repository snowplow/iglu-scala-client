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
import java.net.URL

// Apache Commons
import org.apache.commons.lang3.exception.ExceptionUtils

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
 * Helpers for constructing an HttpRepository.
 * See below for the definition.
 */
object HttpRepositoryRef {

  /**
   * Constructs an EmbeddedRepositoryRef
   * from a JsonNode.
   *
   * @param ref The JSON containing the configuration
   *        for this repository reference
   * @return a configured reference to this embedded
   *         repository
   */
  def parse(ref: JsonNode): ValidatedNel[HttpRepositoryRef] =
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
  def parse(config: JValue): ValidatedNel[HttpRepositoryRef] = {
    
    val conf = RepositoryRefConfig.parse(config)
    val url  = extractUrl(config)
    (conf.toValidationNel |@| url.toValidationNel) { HttpRepositoryRef(_, _) }
  }

  /**
   * Returns the path to this embedded repository.
   *
   * @param ref The JSON containing the configuration
   *        for this repository reference
   * @return the path to the embedded repository on
   *         Success, or an error String on Failure
   */
  // TODO: impl
  private def extractUrl(config: JValue): Validated[URL] =
   	stringToUrl("http://hello.com")

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
  val descriptor = "http"

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
    try {
    	// TODO: fix this. Use a lens?
      val fullPath = s"${uri.toString}/schemas/${schemaKey.toPath}"
      val fullUri  = new URL(fullPath)
      JsonLoader.fromURL(fullUri).some.success
    } catch {
      case e: Throwable => None.success
    }
  }
}
