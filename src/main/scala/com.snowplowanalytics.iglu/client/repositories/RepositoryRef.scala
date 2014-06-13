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
import com.fasterxml.jackson.databind.JsonNode

// JSON Schema
import com.github.fge.jsonschema.core.report.ProcessingMessage

// Scalaz
import scalaz._
import Scalaz._

// json4s
import org.json4s._
import org.json4s.JsonDSL._
import org.json4s.jackson.JsonMethods._

// This project
import validation.ProcessingMessageMethods._

/**
 * Singleton object contains a constructor
 * from a JValue.
 */
object RepositoryRefConfig {

  implicit val formats = DefaultFormats

  /**
   */
  def parse(ref: JValue): Validated[RepositoryRefConfig] =
    ref.extract[RepositoryRefConfig].success
}

/**
 * Common config for RepositoryRef classes.
 */
case class RepositoryRefConfig(
  name: String,
  instancePriority: Int,
  vendorPrefixes: List[String]
  )

/**
 * Common behavior for all RepositoryRef classes.
 */
trait RepositoryRef {

  /**
   * Our configuration for this RepositoryRef
   */
  val config: RepositoryRefConfig

  /**
   * All repositories with a
   * search priority of 1 will be checked before
   * any repository with a search priority of 2.
   */
  val classPriority: Int

  /**
   * Human-readable descriptor for this
   * type of repository ref.
   */
  val descriptor: String

  /**
   * Abstract method. Provide a concrete
   * implementation for how to lookup a schema
   * in this type of repository.
   *
   * @param schemaKey The SchemaKey uniquely identifying
   *        the schema in Iglu
   * @return a Validation boxing either the Schema's
   *         JsonNode on Success, or an error String
   *         on Failure 
   */
  def lookupSchema(schemaKey: SchemaKey): Validated[Option[JsonNode]]

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
    def exception(msg: ProcessingMessage) =
      new RuntimeException(s"Unsafe lookup of schema ${schemaKey} in ${descriptor} Iglu repository ${config.name} failed: ${msg}")

    lookupSchema(schemaKey) match {
      case Success(Some(schema)) => schema
      case Success(None)         => throw exception("not found".toProcessingMessage)
      case Failure(err)          => throw exception(err)
    }
  }

  /**
   * Helper to check if this repository should take
   * priority because of a vendor prefix match. Returns
   * true if we matched our schema's vendor in the
   * list of vendor prefixes.
   *
   * @param schemaKey The SchemaKey uniquely identifying
   *        the schema in Iglu. We will use the vendor
   *        within the SchemaKey to match against our
   *        prefixes
   * @return whether this is a priority lookup or
   *         not
   */
  def vendorMatched(schemaKey: SchemaKey): Boolean = {
    val matches = for {
      p <- config.vendorPrefixes
      m = schemaKey.vendor.startsWith(p)
    } yield m
    matches.foldLeft(false)(_ || _) // True if any match
  }
}
