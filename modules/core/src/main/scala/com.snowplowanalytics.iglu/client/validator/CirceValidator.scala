/*
 * Copyright (c) 2014-2021 Snowplow Analytics Ltd. All rights reserved.
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
import com.snowplowanalytics.iglu.client.resolver.Resolver.SchemaLookupResult

import scala.jdk.CollectionConverters._

// Cats
import cats.Monad
import cats.data.{EitherNel, NonEmptyList}
import cats.syntax.all._

// Jackson
import com.fasterxml.jackson.databind.ObjectMapper

// JSON Schema
import com.networknt.schema._

// circe
import io.circe.Json
import io.circe.jackson.snowplow.circeToJackson

// LruMap
import com.snowplowanalytics.lrumap.{CreateLruMap, LruMap}

import com.snowplowanalytics.iglu.core.SchemaKey
import com.snowplowanalytics.iglu.client.resolver.Resolver.{Cached, NotCached}

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
       |      "minimum": 0
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
       |    "exclusiveMaximum": [ "maximum" ]
       |  },
       |  "default": {}
       |}""".stripMargin

  // These constructors are non-RT because of logging
  private val IgluMetaschema = JsonMetaSchema
    .builder(
      "http://iglucentral.com/schemas/com.snowplowanalytics.self-desc/schema/jsonschema/1-0-0#",
      JsonMetaSchema.getV4
    )
    .addKeyword(new NonValidationKeyword("self"))
    .build()

  private val V4SchemaInstance = JsonSchemaFactory.getInstance(SpecVersion.VersionFlag.V4)

  private val IgluMetaschemaFactory =
    JsonSchemaFactory.builder(V4SchemaInstance).addMetaSchema(IgluMetaschema).build()

  private val SchemaValidatorsConfig: SchemaValidatorsConfig = {
    val config = new SchemaValidatorsConfig()
    // typeLoose is OpenAPI workaround to cast stringly typed properties
    // e.g, with default true "5" string would validate against integer type
    config.setTypeLoose(false)
    config
  }

  private lazy val V4Schema =
    V4SchemaInstance.getSchema(new ObjectMapper().readTree(V4SchemaText))

  def validate(data: Json, schema: Json): Either[ValidatorError, Unit] =
    Either
      .catchNonFatal(
        IgluMetaschemaFactory.getSchema(circeToJackson(schema), SchemaValidatorsConfig)
      )
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
    instance: Json
  ): EitherNel[ValidatorReport, Unit] = {
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

  type InitValidatorCache[F[_]] =
    CreateLruMap[F, (SchemaKey, Int), Either[ValidatorError.InvalidSchema, JsonSchema]]
  def validatorF[F[_]: Monad: InitValidatorCache]: F[ValidatorF[F, Json]] =
    CreateLruMap[F, (SchemaKey, Int), Either[ValidatorError.InvalidSchema, JsonSchema]]
      .create(100)
      .map(validatorFImpl(_))

  def validatorFImpl[F[_]: Monad](
    evaluationCache: LruMap[F, (SchemaKey, Int), Either[ValidatorError.InvalidSchema, JsonSchema]]
  ): ValidatorF[F, Json] =
    new ValidatorF[F, Json] {

      override def validateF(
        data: Json,
        schema: SchemaLookupResult
      ): F[Either[ValidatorError, Unit]] =
        lookupOrEvaluate(schema)
          .map(_.flatMap { jsonschema =>
            validateOnReadySchema(jsonschema, data).leftMap(ValidatorError.InvalidData.apply)
          })

      override def checkSchemaF(
        schema: SchemaLookupResult
      ): F[List[ValidatorError.SchemaIssue]] =
        lookupOrEvaluate(schema)
          .map {
            case Right(_)      => Nil
            case Left(invalid) => invalid.issues.toList
          }

      private def lookupOrEvaluate(
        result: SchemaLookupResult
      ): F[Either[ValidatorError.InvalidSchema, JsonSchema]] = {
        result match {
          case Cached(key, schema, timestamp) =>
            evaluationCache.get((key, timestamp)).flatMap {
              case Some(alreadyEvaluatedSchema) =>
                alreadyEvaluatedSchema.pure[F]
              case None =>
                evaluateSchema(schema)
                  .flatTap(result => evaluationCache.put((key, timestamp), result))
            }
          case NotCached(schema) =>
            evaluateSchema(schema)
        }
      }

      private def evaluateSchema(
        schema: Json
      ): F[Either[ValidatorError.InvalidSchema, JsonSchema]] = {
        Either
          .catchNonFatal(
            IgluMetaschemaFactory
              .getSchema(circeToJackson(schema), SchemaValidatorsConfig)
          )
          .leftMap(ValidatorError.schemaIssue)
          .pure[F]
      }
    }

}
