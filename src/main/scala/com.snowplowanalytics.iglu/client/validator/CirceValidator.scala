/*
 * Copyright (c) 2014-2018 Snowplow Analytics Ltd. All rights reserved.
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
package validator

// Scala
import scala.collection.JavaConverters._

// Cats
import cats.data.{EitherNel, NonEmptyList}
import cats.syntax.all._

// Jackson
import com.fasterxml.jackson.databind.{JsonNode, ObjectMapper}

// JSON Schema
import com.networknt.schema._

// circe
import io.circe.Json

// This project
import ClientError.ValidatorError

object CirceValidator extends Validator[Json] {

  private val IgluMetaschema = JsonMetaSchema
    .builder(
      "http://iglucentral.com/schemas/com.snowplowanalytics.self-desc/schema/jsonschema/1-0-0#",
      JsonMetaSchema.getDraftV4)
    .build()

  private val IgluMetaschemaFactory =
    JsonSchemaFactory.builder(JsonSchemaFactory.getInstance).addMetaSchema(IgluMetaschema).build()

  private lazy val V4MetaschemaJson =
    new ObjectMapper().readTree(getClass.getResourceAsStream("/v4metaschema.json"))

  private lazy val V4Schema = JsonSchemaFactory.getInstance.getSchema(V4MetaschemaJson)

  protected[client] def validate(data: Json, schema: Json): Either[ValidatorError, Unit] =
    Either
      .catchNonFatal(IgluMetaschemaFactory.getSchema(circeToJackson(schema)))
      .leftMap[ValidatorError](ValidatorError.schemaIssue) // Should never be reached in Client
      .flatMap { schema =>
        validateOnReadySchema(schema, data).leftMap(ValidatorError.InvalidData.apply)
      }

  def checkSchema(schema: Json): List[ValidatorError.SchemaIssue] = {
    val jacksonJson = new ObjectMapper().readTree(schema.noSpaces)
    V4Schema
      .validate(jacksonJson)
      .asScala
      .toList
      .map(m => ValidatorError.SchemaIssue(m.getPath, m.getMessage))
  }

  /** Validate instance against schema and return same instance */
  private def validateOnReadySchema(
    schema: JsonSchema,
    instance: Json): EitherNel[ValidatorReport, Unit] = {
    val messages = schema
      .validate(circeToJackson(instance))
      .asScala
      .toList
      .map(fromValidationMessage)

    messages match {
      case x :: xs => NonEmptyList(x, xs).asLeft
      case Nil     => ().asRight
    }
  }

  private val mapper = new ObjectMapper()

  private def fromValidationMessage(m: ValidationMessage): ValidatorReport =
    ValidatorReport(m.getMessage, m.getPath.some, m.getArguments.toList, m.getType.some)

  private def circeToJackson(circeJson: Json): JsonNode =
    mapper.readTree(circeJson.noSpaces)

}
