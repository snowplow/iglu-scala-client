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
import com.fasterxml.jackson.databind.ObjectMapper

// JSON Schema
import com.networknt.schema._

// circe
import io.circe.Json
import io.circe.jackson.circeToJackson

object CirceValidator extends Validator[Json] {

  private val V4SchemaText =
    """|{
       |  "id": "http://json-schema.org/draft-04/schema#",
       |  "$schema": "http://json-schema.org/draft-04/schema#",
       |  "description": "Core schema meta-schema",
       |  "definitions": {
       |    "schemaArray": {
       |      "type": "array",
       |      "minItems": 1,
       |      "items": { "$ref": "#" }
       |    },
       |    "positiveInteger": {
       |      "type": "integer",
       |      "minimum": 0
       |    },
       |    "positiveIntegerDefault0": {
       |      "allOf": [ { "$ref": "#/definitions/positiveInteger" }, { "default": 0 } ]
       |    },
       |    "simpleTypes": {
       |      "enum": [ "array", "boolean", "integer", "null", "number", "object", "string" ]
       |    },
       |    "stringArray": {
       |      "type": "array",
       |      "items": { "type": "string" },
       |      "minItems": 1,
       |      "uniqueItems": true
       |    }
       |  },
       |  "type": "object",
       |  "properties": {
       |    "id": {
       |      "type": "string"
       |    },
       |    "$schema": {
       |      "type": "string"
       |    },
       |    "title": {
       |      "type": "string"
       |    },
       |    "description": {
       |      "type": "string"
       |    },
       |    "default": {},
       |    "multipleOf": {
       |      "type": "number",
       |      "minimum": 0,
       |      "exclusiveMinimum": true
       |    },
       |    "maximum": {
       |      "type": "number"
       |    },
       |    "exclusiveMaximum": {
       |      "type": "boolean",
       |      "default": false
       |    },
       |    "minimum": {
       |      "type": "number"
       |    },
       |    "exclusiveMinimum": {
       |      "type": "boolean",
       |      "default": false
       |    },
       |    "maxLength": { "$ref": "#/definitions/positiveInteger" },
       |    "minLength": { "$ref": "#/definitions/positiveIntegerDefault0" },
       |    "pattern": {
       |      "type": "string",
       |      "format": "regex"
       |    },
       |    "additionalItems": {
       |      "anyOf": [
       |        { "type": "boolean" },
       |        { "$ref": "#" }
       |      ],
       |      "default": {}
       |    },
       |    "items": {
       |      "anyOf": [
       |        { "$ref": "#" },
       |        { "$ref": "#/definitions/schemaArray" }
       |      ],
       |      "default": {}
       |    },
       |    "maxItems": { "$ref": "#/definitions/positiveInteger" },
       |    "minItems": { "$ref": "#/definitions/positiveIntegerDefault0" },
       |    "uniqueItems": {
       |      "type": "boolean",
       |      "default": false
       |    },
       |    "maxProperties": { "$ref": "#/definitions/positiveInteger" },
       |    "minProperties": { "$ref": "#/definitions/positiveIntegerDefault0" },
       |    "required": { "$ref": "#/definitions/stringArray" },
       |    "additionalProperties": {
       |      "anyOf": [
       |        { "type": "boolean" },
       |        { "$ref": "#" }
       |      ],
       |      "default": {}
       |    },
       |    "definitions": {
       |      "type": "object",
       |      "additionalProperties": { "$ref": "#" },
       |      "default": {}
       |    },
       |    "properties": {
       |      "type": "object",
       |      "additionalProperties": { "$ref": "#" },
       |      "default": {}
       |    },
       |    "patternProperties": {
       |      "type": "object",
       |      "additionalProperties": { "$ref": "#" },
       |      "default": {}
       |    },
       |    "dependencies": {
       |      "type": "object",
       |      "additionalProperties": {
       |        "anyOf": [
       |          { "$ref": "#" },
       |          { "$ref": "#/definitions/stringArray" }
       |        ]
       |      }
       |    },
       |    "enum": {
       |      "type": "array",
       |      "minItems": 1,
       |      "uniqueItems": true
       |    },
       |    "type": {
       |      "anyOf": [
       |        { "$ref": "#/definitions/simpleTypes" },
       |        {
       |          "type": "array",
       |          "items": { "$ref": "#/definitions/simpleTypes" },
       |          "minItems": 1,
       |          "uniqueItems": true
       |        }
       |      ]
       |    },
       |    "format": { "type": "string" },
       |    "allOf": { "$ref": "#/definitions/schemaArray" },
       |    "anyOf": { "$ref": "#/definitions/schemaArray" },
       |    "oneOf": { "$ref": "#/definitions/schemaArray" },
       |    "not": { "$ref": "#" }
       |  },
       |  "dependencies": {
       |    "exclusiveMaximum": [ "maximum" ],
       |    "exclusiveMinimum": [ "minimum" ]
       |  },
       |  "default": {}
       |}""".stripMargin

  private val IgluMetaschema = JsonMetaSchema
    .builder(
      "http://iglucentral.com/schemas/com.snowplowanalytics.self-desc/schema/jsonschema/1-0-0#",
      JsonMetaSchema.getDraftV4)
    .build()

  private val IgluMetaschemaFactory =
    JsonSchemaFactory.builder(JsonSchemaFactory.getInstance).addMetaSchema(IgluMetaschema).build()

  private lazy val V4Schema =
    JsonSchemaFactory.getInstance.getSchema(new ObjectMapper().readTree(V4SchemaText))

  def validate(data: Json, schema: Json): Either[ValidatorError, Unit] =
    Either
      .catchNonFatal(IgluMetaschemaFactory.getSchema(circeToJackson(schema)))
      .leftMap[ValidatorError](ValidatorError.schemaIssue) // Should never be reached in Client
      .flatMap { schema =>
        validateOnReadySchema(schema, data).leftMap(ValidatorError.InvalidData.apply)
      }

  def checkSchema(schema: Json): List[ValidatorError.SchemaIssue] = {
    val jacksonJson = circeToJackson(schema)
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

  private def fromValidationMessage(m: ValidationMessage): ValidatorReport =
    ValidatorReport(m.getMessage, m.getPath.some, m.getArguments.toList, m.getType.some)
}
