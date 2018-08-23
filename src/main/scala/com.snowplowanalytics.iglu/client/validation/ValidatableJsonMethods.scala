/*
 * Copyright (c) 2014-2017 Snowplow Analytics Ltd. All rights reserved.
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
package validation

// Scala
import cats.data.StateT

import scala.collection.JavaConversions._

// Jackson
import com.fasterxml.jackson.databind.JsonNode

// JSON Schema
import com.github.fge.jsonschema.SchemaVersion
import com.github.fge.jsonschema.cfg.ValidationConfiguration
import com.github.fge.jsonschema.core.report.{ListReportProvider, LogLevel, ProcessingMessage}
import com.github.fge.jsonschema.main.{JsonSchemaFactory, JsonValidator}

// Cats
import cats._
import cats.implicits._
import cats.data.NonEmptyList

// This project
import ProcessingMessageMethods._

object ValidatableJsonMethods extends Validatable[JsonNode] {

  private[validation] lazy val JsonSchemaValidator = getJsonSchemaValidator(SchemaVersion.DRAFTV4)

  def validateAgainstSchema(instance: JsonNode, schema: JsonNode)(
    implicit resolver: Resolver): ValidatedNelType[JsonNode] = {
    val validatedReport = try {
      JsonSchemaValidator.validateUnchecked(schema, instance).valid
    } catch {
      case re: RuntimeException =>
        s"Exception validating instance, possibly caused by malformed $schema field: [$re]".toProcessingMessageNel.invalid
    }
    validatedReport.andThen { report =>
      val msgs = report.iterator.toList
      msgs match {
        case x :: xs if !report.isSuccess => NonEmptyList(x, xs).invalid
        case Nil if report.isSuccess      => instance.valid
        case _ =>
          throw new RuntimeException(
            s"Validation report success [$report.isSuccess] conflicts with message count [$msgs.length]")
      }
    }
  }

  def validate(instance: JsonNode, dataOnly: Boolean = false)(
    implicit resolver: Resolver): ValidatedNelType[JsonNode] = {

    validateAsSelfDescribing(instance)
      .andThen { j =>
        val key  = j.get("schema").asText
        val data = j.get("data")

        resolver.lookupSchema(key).map(schema => (data, schema))
      }
      .andThen {
        case (data, schema) =>
          validateAgainstSchema(data, schema).map(_ => if (dataOnly) data else instance)
      }
  }

  def validateAndIdentifySchema(instance: JsonNode, dataOnly: Boolean = false)(
    implicit resolver: Resolver): ValidatedNelType[JsonSchemaPair] = {

    validateAsSelfDescribing(instance)
      .andThen { j =>
        val key  = j.get("schema").asText
        val data = j.get("data")

        SchemaKey
          .parseNel(key)
          .andThen(
            schemaKey =>
              resolver
                .lookupSchema(schemaKey)
                .andThen(schema => validateAgainstSchema(data, schema))
                .map(_ => if (dataOnly) (schemaKey, data) else (schemaKey, instance)))
      }
  }

  def verifySchemaAndValidate(
    instance: JsonNode,
    schemaCriterion: SchemaCriterion,
    dataOnly: Boolean = false)(implicit resolver: Resolver): ValidatedNelType[JsonNode] = {

    validateAsSelfDescribing(instance)
      .andThen { j =>
        val key  = j.get("schema").asText
        val data = j.get("data")

        SchemaKey
          .parseNel(key)
          .andThen(
            schemaKey =>
              if (schemaCriterion.matches(schemaKey))
                schemaKey.valid
              else
                s"Verifying schema as $schemaCriterion failed: found $schemaKey".toProcessingMessageNel.invalid)
          .andThen(schemaKey => resolver.lookupSchema(schemaKey))
          .andThen(schema => validateAgainstSchema(data, schema))
          .map(_ => if (dataOnly) data else instance)
      }
  }

  /**
   * Get our schema for self-describing Iglu instances.
   *
   * Unsafe lookup is fine here because we know this
   * schema exists in our resources folder
   */
  private[validation] def getSelfDescribingSchema(implicit resolver: Resolver): JsonNode =
    resolver.unsafeLookupSchema(
      SchemaKey("com.snowplowanalytics.self-desc", "instance-iglu-only", "jsonschema", "1-0-0")
    )

  /**
   * Validates that this JSON is a self-
   * describing JSON.
   *
   * @param instance The JSON to check
   * @return either Success boxing the
   *         JsonNode, or a Failure boxing
   *         a NonEmptyList of
   *         ProcessingMessages
   */
  private[validation] def validateAsSelfDescribing(instance: JsonNode)(
    implicit resolver: Resolver): ValidatedNelType[JsonNode] =
    validateAgainstSchema(instance, getSelfDescribingSchema)

  /**
   * Factory for retrieving a JSON Schema
   * validator with the specific version.
   *
   * @param version The version of the JSON
   *        Schema spec to validate against
   * @return a JsonValidator
   */
  private[validation] def getJsonSchemaValidator(version: SchemaVersion): JsonValidator = {

    // Override the ReportProvider so we never throw Exceptions and only collect ERRORS+
    val rep = new ListReportProvider(LogLevel.ERROR, LogLevel.NONE)
    val cfg = ValidationConfiguration.newBuilder
      .setDefaultVersion(version)
      .freeze
    val fac = JsonSchemaFactory.newBuilder
      .setReportProvider(rep)
      .setValidationConfiguration(cfg)
      .freeze

    fac.getValidator
  }
}
