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
import com.snowplowanalytics.iglu.client.utils.SchemaKeyUtils

import scala.collection.JavaConverters._

// Jackson
import com.fasterxml.jackson.databind.ObjectMapper

// circe
import io.circe.Json
import io.circe.optics.JsonPath._

// JSON Schema
import com.networknt.schema._

// Cats
import cats.syntax.either._
import cats.syntax.validated._
import cats.data.NonEmptyList

// Iglu Core
import com.snowplowanalytics.iglu.core.{SchemaCriterion, SchemaKey, SchemaVer}

// This project
import ProcessingMessageMethods._
import utils.JacksonCatsUtils.circeToJackson

object ValidatableCirceMethods extends Validatable[Json] {

  private val metaSchema = JsonMetaSchema
    .builder(
      "http://iglucentral.com/schemas/com.snowplowanalytics.self-desc/schema/jsonschema/1-0-0#",
      JsonMetaSchema.getDraftV4)
    .build()

  private val factory =
    JsonSchemaFactory.builder(JsonSchemaFactory.getInstance).addMetaSchema(metaSchema).build()

  private def validateOnReadySchema(schema: JsonSchema, instance: Json): ValidatedNelType[Json] = {
    val messages = schema
      .validate(circeToJackson(instance))
      .asScala
      .toList
      .map(message => ProcessingMessage(message.getMessage))

    messages match {
      case x :: xs => NonEmptyList(x, xs).invalid
      case Nil     => instance.valid
    }
  }

  def validateAgainstSchema(instance: Json, schemaJson: Json): ValidatedNelType[Json] = {
    Either
      .catchNonFatal(factory.getSchema(circeToJackson(schemaJson)))
      .leftMap(error => NonEmptyList.one(ProcessingMessage(error.getMessage)))
      .toValidated
      .andThen(schema => validateOnReadySchema(schema, instance))
  }

  def validate(instance: Json, dataOnly: Boolean = false)(
    implicit resolver: Resolver): ValidatedNelType[Json] = {

    validateAsSelfDescribing(instance)
      .andThen { json =>
        val keyOpt  = root.schema.string.getOption(json)
        val dataOpt = root.data.json.getOption(json)

        (keyOpt product dataOpt).toValidNel(s"Malformed JSON: ${json.spaces2}".toProcessingMessage)
      }
      .andThen { case (key, data) => resolver.lookupSchema(key).map(schema => (data, schema)) }
      .andThen {
        case (data, schema) =>
          validateAgainstSchema(data, schema).map(_ => if (dataOnly) data else instance)
      }
  }

  def validateAndIdentifySchema(instance: Json, dataOnly: Boolean = false)(
    implicit resolver: Resolver): ValidatedNelType[(SchemaKey, Json)] = {

    validateAsSelfDescribing(instance)
      .andThen { json =>
        val keyOpt  = root.schema.string.getOption(json)
        val dataOpt = root.data.json.getOption(json)

        (keyOpt product dataOpt).toValidNel(s"Malformed JSON: ${json.spaces2}".toProcessingMessage)
      }
      .andThen {
        case (key, data) =>
          SchemaKeyUtils
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
    instance: Json,
    schemaCriterion: SchemaCriterion,
    dataOnly: Boolean = false)(implicit resolver: Resolver): ValidatedNelType[Json] = {

    validateAsSelfDescribing(instance)
      .andThen { json =>
        val keyOpt  = root.schema.string.getOption(json)
        val dataOpt = root.data.json.getOption(json)

        (keyOpt product dataOpt).toValidNel(s"Malformed JSON: ${json.spaces2}".toProcessingMessage)
      }
      .andThen {
        case (key, data) =>
          SchemaKeyUtils
            .parseNel(key)
            .andThen(schemaKey =>
              if (schemaCriterion.matches(schemaKey))
                schemaKey.valid
              else
                s"Verifying schema as ${schemaCriterion.asString} failed: found ${schemaKey.toSchemaUri}".toProcessingMessageNel.invalid)
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
  private[validation] def getSelfDescribingSchema(implicit resolver: Resolver): Json =
    resolver.unsafeLookupSchema(
      SchemaKey(
        "com.snowplowanalytics.self-desc",
        "instance-iglu-only",
        "jsonschema",
        SchemaVer.Full(1, 0, 0))
    )

  /**
   * Validates that this JSON is a self-
   * describing JSON.
   *
   * @param instance The JSON to check
   * @return either Success boxing the
   *         Json, or a Failure boxing
   *         a NonEmptyList of
   *         ProcessingMessages
   */
  private[validation] def validateAsSelfDescribing(instance: Json)(
    implicit resolver: Resolver): ValidatedNelType[Json] =
    validateAgainstSchema(instance, getSelfDescribingSchema)
}
