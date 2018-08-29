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

// Cats
import cats.syntax.option._
import cats.syntax.validated._

import validation.ProcessingMessageMethods._

/**
 * Companion object containing alternative constructor for a SchemaCriterion.
 */
object SchemaCriterion {

  private val SchemaCriterionRegex =
    "^iglu:([a-zA-Z0-9-_.]+)/([a-zA-Z0-9-_]+)/([a-zA-Z0-9-_]+)/([0-9]+)-([0-9]+|\\*)-([0-9]+|\\*)$".r

  /**
   * Try to parse string into SchemaCriterion
   * an Iglu-format for schema URI wildcard, which looks like:
   *
   * iglu:com.snowplowanalytics.snowplow/mobile_context/jsonschema/1-*-*
   * iglu:com.snowplowanalytics.snowplow/mobile_context/jsonschema/1-0-*
   * iglu:com.snowplowanalytics.snowplow/mobile_context/jsonschema/1-*-0
   * iglu:com.snowplowanalytics.snowplow/mobile_context/jsonschema/1-0-0
   *
   * @param schemaCriterion An Iglu-format schema URI
   * @return a Validation-boxed SchemaCriterion for
   *         Success, and an error String on Failure
   */
  def parse(schemaCriterion: String): ValidatedType[SchemaCriterion] = schemaCriterion match {
    case SchemaCriterionRegex(vnd, n, f, mod, "*", add) if add == "*" =>
      SchemaCriterion(vnd, n, f, mod.toInt, None, None).valid
    case SchemaCriterionRegex(vnd, n, f, mod, "*", add) =>
      SchemaCriterion(vnd, n, f, mod.toInt, None, add.toInt.some).valid
    case SchemaCriterionRegex(vnd, n, f, mod, rev, "*") =>
      SchemaCriterion(vnd, n, f, mod.toInt, rev.toInt.some, None).valid
    case SchemaCriterionRegex(vnd, n, f, mod, rev, add) =>
      SchemaCriterion(vnd, n, f, mod.toInt, rev.toInt.some, add.toInt.some).valid
    case _ =>
      s"$schemaCriterion is not a valid Iglu-format schema criterion".toProcessingMessage.invalid
  }

  def parseNel(schemaCriterion: String): ValidatedNelType[SchemaCriterion] =
    parse(schemaCriterion).toValidatedNel

  /**
   * Constructs an exhaustive SchemaCriterion.
   *
   * @return our constructed SchemaCriterion
   */
  def apply(
    vendor: String,
    name: String,
    format: String,
    model: Int,
    revision: Int,
    addition: Int): SchemaCriterion =
    SchemaCriterion(vendor, name, format, model, revision.some, addition.some)

  /**
   * Constructs a SchemaCriterion from everything
   * except the addition.
   *
   * @return our constructed SchemaCriterion
   */
  def apply(
    vendor: String,
    name: String,
    format: String,
    model: Int,
    revision: Int): SchemaCriterion =
    SchemaCriterion(vendor, name, format, model, revision.some)

  /**
   * Constructs a SchemaCriterion which is agnostic
   * of addition and revision.
   * Restricts to model only.
   *
   * @return our constructed SchemaCriterion
   */
  def apply(vendor: String, name: String, format: String, model: Int): SchemaCriterion =
    SchemaCriterion(vendor, name, format, model, None, None)
}

/**
 * Class to validate SchemaKeys.
 */
case class SchemaCriterion(
  vendor: String,
  name: String,
  format: String,
  model: Int,
  revision: Option[Int] = None,
  addition: Option[Int] = None) {

  lazy val versionString =
    "%s-%s-%s".format(model, revision.getOrElse("*"), addition.getOrElse("*"))

  /**
   * Whether the vendor, name, and format are all correct.
   *
   * @param key The SchemaKey to validate
   * @return whether the first three fields are correct
   */
  private def prefixMatches(key: SchemaKey): Boolean =
    key.vendor == vendor && key.name == name && key.format == format

  /**
   * Whether a SchemaKey is valid.
   *
   * It's valid if the vendor, name, format, and model all match
   * and the supplied key's revision and addition do not exceed the
   * criterion's revision and addition.
   *
   * This comparator will continue to function correctly when
   * revisions are deprecated.
   *
   * @param key The SchemaKey to validate
   * @return whether the SchemaKey is valid
   */
  def matches(key: SchemaKey): Boolean = {
    prefixMatches(key) && {
      key.getModelRevisionAddition match {
        case None => false
        case Some((keyModel, keyRevision, keyAddition)) =>
          keyModel == model && (revision match {
            case None =>
              addition match {
                case Some(a) => a == keyAddition
                case None    => true
              }
            case Some(r) =>
              addition match {
                case None    => keyRevision <= r
                case Some(a) => keyRevision < r || (keyRevision == r && keyAddition <= a)
              }
          })
      }
    }
  }

  /**
   * Format as a schema URI, but the revision and addition
   * may be replaced with "*" wildcards.
   *
   * @return the String representation of this
   *         SchemaKey
   */
  override def toString = s"iglu:$vendor/$name/$format/$versionString"
}
