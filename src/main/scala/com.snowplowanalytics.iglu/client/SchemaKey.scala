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
 * Companion object contains a custom constructor for
 * an Iglu SchemaKey.
 */
object SchemaKey {

  private val SchemaUriRegex = "^iglu:([a-zA-Z0-9-_.]+)/([a-zA-Z0-9-_]+)/([a-zA-Z0-9-_]+)/([0-9]+-[0-9]+-[0-9]+)$".r

  /**
   * Custom constructor for an Iglu SchemaKey from
   * an Iglu-format schema URI, which looks like:
   *
   * iglu://com.snowplowanalytics.snowplow/mobile_context/jsonschema/1-0-0
   *
   * @param schemaUri An Iglu-format schema URI
   * @return a Validation-boxed SchemaKey for
   *         Success, and an error String on Failure
   */
  def apply(schemaUri: String): Validated[SchemaKey] = schemaUri match {
    case SchemaUriRegex(vnd, n, f, ver) =>
      SchemaKey(vnd, n, f, ver).success
    case _ =>
      s"$schemaUri is not a valid Iglu-format schema URI".fail.toProcessingMessage
  }
}

/**
 * The four elements of any Iglu-compatible schema
 * key:
 *
 * 1. vendor
 * 2. name
 * 3. format
 * 4. version
 */
case class SchemaKey(
  val vendor: String,
  val name: String,
  val format: String,
  val version: SchemaVer) {

  /**
   * Converts a SchemaKey into a Jackson JsonNode
   * containing each element. The properties in this
   * JSON conform to the self-describing JSON schema.
   *
   * @return the SchemaKey as a JsonNode
   */
  def toJsonNode: JsonNode =
    asJsonNode(this.toJValue)

  /**
   * Converts a SchemaKey into a json4s JValue
   * containing each element. The properties in this
   * JSON conform to the self-describing JSON schema.
   *
   * @return the SchemaKey as a JValue
   */
  def toJValue: JValue =
    ("vendor"  -> vendor) ~
    ("name"    -> name) ~
    ("format"  -> format) ~
    ("version" -> version)

  /**
   * Converts a SchemaKey into a path which is compatible
   * with most local and remote Iglu schema repositories.
   *
   * @return a path usable for addressing local and remote
   *         Iglu schema lookups
   */
  def toPath: String =
    s"$vendor/$name/$format/$version"

  /**
   * Converts the SchemaKey back to an Iglu-format
   * schema URI.
   *
   * @return the SchemaKey as a Iglu-format schema
   *         URI.
   */
  def toSchemaUri: String =
    s"iglu:${toPath}"

  /**
   * The optimal String representation of a SchemaKey
   * is as an Iglu-format schema URI.
   *
   * @return the String representation of this
   *         SchemaKey
   */
  override def toString: String =
    toSchemaUri
}
