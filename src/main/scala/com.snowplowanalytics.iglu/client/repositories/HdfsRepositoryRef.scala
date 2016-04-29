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
import java.io.{InputStreamReader, BufferedReader, IOException}
import java.net.URI


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

// Hadoop
import org.apache.hadoop.fs._;
import org.apache.hadoop.conf._;
import org.apache.hadoop.io._;
import org.apache.hadoop.util._;

/**
 * Helpers for constructing an HdfsRepository.
 * See below for the definition.
 */
object HdfsRepositoryRef {

  implicit val formats = DefaultFormats

  /**
   * Sniffs a config JSON to determine if this is
   * an hdfs repository ref or not.
   *
   * @param config The configuration JSON to sniff
   * @return true if this is the configuration for
   *         an HdfsRepositoryRef, else false
   */
  def isHdfs(config: JValue): Boolean =
    (config \ "connection" \ "hdfs").toSome.isDefined

  /**
   * Constructs an HdfsRepositoryRef
   * from a JsonNode.
   *
   * @param config The JSON containing the configuration
   *        for this repository reference
   * @return a configured reference to this hdfs
   *         repository
   */
  def parse(config: JsonNode): ValidatedNel[HdfsRepositoryRef] =
    parse(fromJsonNode(config))

  /**
   * Constructs an HdfsRepositoryRef
   * from a JValue.
   *
   * @param config The JSON containing the configuration
   *        for this repository reference
   * @return a configured reference to this hdfs
   *         repository
   */
  def parse(config: JValue): ValidatedNel[HdfsRepositoryRef] = {
    val conf = RepositoryRefConfig.parse(config)
    val path = extractPath(config)
    (conf |@| path.toValidationNel) { HdfsRepositoryRef(_, _) }
  }

  /**
   * Returns the path to this hdfs repository.
   *
   * @param ref The JSON containing the configuration
   *        for this repository reference
   * @return the path to the hdfs repository on
   *         Success, or an error String on Failure
   */
  private def extractPath(config: JValue): Validated[String] =
    try {
      (config \ "connection" \ "hdfs" \ "path").extract[String].success
    } catch {
      case me: MappingException => s"Could not extract connection.hdfs.path from ${compact(render(config))}".fail.toProcessingMessage
    }

}

case class HdfsRepositoryRef(
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
  val descriptor = "hdfs"

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
      val pt:Path = new Path(schemaPath)
      var conf = new Configuration()
      if (path.startsWith("hdfs:")) {
        conf.addResource(new Path("/etc/hadoop/conf/core-site.xml"))
        conf.addResource(new Path("/etc/hadoop/conf/hdfs-site.xml"))
      }
      val fs:FileSystem = FileSystem.get(conf)
      val br=new BufferedReader(new InputStreamReader(fs.open(pt)));
      JsonLoader.fromReader(br).some.success
    } catch {
      case jpe: JsonParseException => // Child of IOException so match first
        s"Problem parsing ${schemaPath} as JSON in ${descriptor} Iglu repository ${config.name}: %s".format(VE.stripInstanceEtc(jpe.getMessage)).fail.toProcessingMessage
      case ioe: IOException =>
        None.success // Schema not found
      case e: Throwable =>
        s"Unknown problem reading and parsing ${schemaPath} in ${descriptor} Iglu repository ${config.name}: ${VE.getThrowableMessage(e)}".fail.toProcessingMessage
    }
  }
}
