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
import java.io.FileNotFoundException
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

// Json Schema
import com.github.fge.jackson.JsonNodeReader

// Scala
import scala.util.control.NonFatal

// Scalaz
import scalaz._
import Scalaz._

// json4s
import org.json4s._
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

  private lazy val reader = new JsonNodeReader()

  /**
   * Helper class to extract HTTP URI and api key from config JSON
   */
  private[client] case class HttpConnection(uri: String, apikey: Option[String])

  /**
   * Read a JsonNode from an URL using optional apikey
   * This method is just a copy of [[com.github.fge.jackson.JsonLoader.fromURL]]
   * with added optional header, so it is unsafe as well and throws same exceptions
   *
   * @param url the URL to fetch the JSON document from
   * @param apikey optional apikey UUID to aunthenticate in Iglu HTTP repo
   * @return The document at that URL
   */
  private def getFromUrl(url: URL, apikey: Option[String]): JsonNode = {
    val connection = url.openConnection()
    apikey match {
      case Some(key) => connection.setRequestProperty("apikey", key)
      case None => ()
    }
    reader.fromInputStream(connection.getInputStream)
  }


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
    val http  = extractUrl(config)

    (conf |@| http.toValidationNel) { (c, h) => HttpRepositoryRef(c, h.uri, h.apikey) }
  }

  /**
   * Returns the path to this embedded repository.
   *
   * @param config The JSON containing the configuration
   *        for this repository reference
   * @return the path to the embedded repository on
   *         Success, or an error String on Failure
   */
  private def extractUrl(config: JValue): Validated[HttpConnection] =
    try {
      (config \ "connection" \ "http").extract[HttpConnection].success
    } catch {
      case me: MappingException => s"Could not extract connection.http from ${compact(render(config))}".failure.toProcessingMessage
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
      case npe: NullPointerException => "Provided URL was null".failure
      case mue: MalformedURLException => "Provided URL string [%s] is malformed: [%s]".format(url, mue.getMessage).failure
      case iae: IllegalArgumentException => "Provided URL string [%s] violates RFC 2396: [%s]".format(url, ExceptionUtils.getRootCause(iae).getMessage).failure
      case e: Throwable => "Unexpected error creating URL from string [%s]: [%s]".format(url, e.getMessage).failure
    }).toProcessingMessage

}

/**
 * An HTTP repository is one which is accessible over
 * HTTP.
 */
case class HttpRepositoryRef(
  override val config: RepositoryRefConfig,
  uri: String, apikey: Option[String] = None) extends RepositoryRef {

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
  // TODO: this is only intermittently working when there is a network outage (e.g. running test suite on Tube)
  def lookupSchema(schemaKey: SchemaKey): Validated[Option[JsonNode]] = {
    try {
      for {
        url <- HttpRepositoryRef.stringToUrl(s"$uri/schemas/${schemaKey.toPath}")
        sch = HttpRepositoryRef.getFromUrl(url, apikey).some
      } yield sch
    } catch {
      // The most common failure case: the schema is not found in the repo
      case fnf: FileNotFoundException => None.success
      case jpe: JsonParseException =>
        s"Problem parsing ${schemaKey} as JSON in ${descriptor} Iglu repository ${config.name}: %s".format(VE.stripInstanceEtc(jpe.getMessage)).failure.toProcessingMessage
      case uhe: UnknownHostException =>
        s"Unknown host issue fetching ${schemaKey} in ${descriptor} Iglu repository ${config.name}: ${uhe.getMessage}".failure.toProcessingMessage
      case NonFatal(nfe) =>
        s"Unexpected exception fetching $schemaKey in ${descriptor} Iglu repository ${config.name}: $nfe".failure.toProcessingMessage
    }
  }
}
