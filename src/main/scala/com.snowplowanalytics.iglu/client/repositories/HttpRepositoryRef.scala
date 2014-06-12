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
  def apply(ref: JsonNode): Validation[String, HttpRepositoryRef] =
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
  def apply(config: JValue): Validation[String, HttpRepositoryRef] =
    for {
      conf <- RepositoryRefConfig(config)
      uri  <- extractUri(config)
    } yield HttpRepositoryRef(conf, uri)

  /**
   * Returns the path to this embedded repository.
   *
   * @param ref The JSON containing the configuration
   *        for this repository reference
   * @return the path to the embedded repository on
   *         Success, or an error String on Failure
   */
  // TODO: implement this properly
  def extractUri(config: JValue): Validation[String, URL] =
   	(new URL("http://hello.com")).success

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
   * a JsonNode. Unsafe - only use when you know the
   * schema is available locally.
   *
   * @param schemaKey The SchemaKey uniquely identifies
   *        the schema in Iglu
   * @return the JsonNode representing this schema
   */
  def unsafeLookupSchema(schemaKey: SchemaKey): JsonNode = {
    // TODO: fix this. Use a lens?
    val fullPath = s"${uri.toString}/schemas/${schemaKey.toPath}"
    val fullUri  = new URL(fullPath)
    JsonLoader.fromURL(fullUri)
  }
}
