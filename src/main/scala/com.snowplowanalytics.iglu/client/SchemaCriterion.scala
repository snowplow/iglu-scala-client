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

// Scalaz
import scalaz._
import Scalaz._

/**
 * Companion object containing alternative constructor for a SchemaCriterion.
 */
object SchemaCriterion {

  def apply(vendor: String, name: String, format: String, model: Int, revision: Int, addition: Int): SchemaCriterion =
    SchemaCriterion(vendor, name, format, model, revision.some, addition.some)

  // TODO: decide whether this should overload should instead be used to create a SchemaCriterion with only a model and addition
  def apply(vendor: String, name: String, format: String, model: Int, revision: Int): SchemaCriterion =
    SchemaCriterion(vendor, name, format, model, revision.some, None)

  def apply(vendor: String, name: String, format: String, model: Int): SchemaCriterion =
    SchemaCriterion(vendor, name, format, model, None, None)
}

/**
 * Class to validate SchemaKeys
 */
case class SchemaCriterion(
  val vendor: String,
  val name: String,
  val format: String,
  val model: Int,
  val revision: Option[Int] = None,
  val addition: Option[Int] = None) {

  lazy val versionString = "%s-%s-%s".format(model, revision.getOrElse("x"), addition.getOrElse("x"))

  /**
   * Whether the vendor, name, and format are all correct
   *
   * @param key The SchemaKey to validate
   * @return Whether the first three fields are correct
   */
  private def prefixMatches(key: SchemaKey): Boolean =
    key.vendor == vendor && key.name == name && key.format == format

  /**
   * Whether a SchemaKey is valid
   * It's valid if the vendor, name, format, and model all match
   * and the actual revision and addition do not exceed the
   * expected revision and addition
   *
   * @param key The SchemaKey to validate
   * @return Whether the SchemaKey is valid
   */
  def matches(key: SchemaKey): Boolean = {
    prefixMatches(key) && {
      key.getModelRevisionAddition match {
        case None => false
        case Some((keyModel, keyRevision, keyAddition)) => {
          keyModel == model && (addition match {
              case None => revision match {
                case None => true
                case Some(r) => keyRevision <= r
              }
              case Some(a) => revision match {

                // If we are using revisionless SchemaVer, treat the expected revision as 0
                case None => keyRevision == 0 && keyAddition <= a

                // If the acutal revision is less than the expected revision, pass;
                // otherwise only pass if the revisions are the same and the expected
                // addition exceeds the actual addition
                case Some(r) => keyRevision < r || (keyRevision == r && keyAddition <= a)
              }
            })
        }
      }
    }
  }

  /**
   * Format as a schema URI, but the revision and addition
   * may be replaced with "x".
   *
   * @return the String representation of this
   *         SchemaKey
   */
  override def toString = s"iglu:$vendor/$name/$format/$versionString"
}
