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
import scala.collection.JavaConverters._

// Jackson
import com.fasterxml.jackson.databind.JsonNode

// JSON Schema
import com.networknt.schema._

// Cats
import cats._
import cats.implicits._
import cats.data.NonEmptyList

// This project
import ProcessingMessageMethods._

object ValidatableJsonMethods extends Validatable[JsonNode] {

  private val metaSchema = JsonMetaSchema
    .builder(
      "http://iglucentral.com/schemas/com.snowplowanalytics.self-desc/schema/jsonschema/1-0-0#",
      JsonMetaSchema.getDraftV4)
    .build()

  private val factory =
    JsonSchemaFactory.builder(JsonSchemaFactory.getInstance).addMetaSchema(metaSchema).build()

  private def validateOnReadySchema(
    schema: JsonSchema,
    instance: JsonNode): ValidatedNelType[JsonNode] = {
    val messages = schema
      .validate(instance)
      .asScala
      .toList
      .map(message => ProcessingMessage(message.getMessage))

    messages match {
      case x :: xs => NonEmptyList(x, xs).invalid
      case Nil     => instance.valid
    }
  }

  def validateAgainstSchema(
    instance: JsonNode,
    schemaJson: JsonNode): ValidatedNelType[JsonNode] = {
    Either
      .catchNonFatal(factory.getSchema(schemaJson))
      .leftMap(error => NonEmptyList.one(ProcessingMessage(error.getMessage)))
      .toValidated
      .andThen(schema => validateOnReadySchema(schema, instance))
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
}
