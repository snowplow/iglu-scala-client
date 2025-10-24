/*
 * Copyright (c) 2014-2023 Snowplow Analytics Ltd. All rights reserved.
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
import com.snowplowanalytics.iglu.client.resolver.Resolver.ResolverResult
import com.snowplowanalytics.iglu.client.resolver.StorageTime
import com.snowplowanalytics.iglu.core.circe.MetaSchemas
// Scala
import com.fasterxml.jackson.databind.JsonNode
import com.networknt.schema.resource.{InputStreamSource, SchemaLoader}
import com.networknt.schema.AbsoluteIri
import com.snowplowanalytics.iglu.client.resolver.Resolver.{SchemaItem, SchemaLookupResult}
import java.io.ByteArrayInputStream
import java.nio.charset.StandardCharsets
import scala.jdk.CollectionConverters._

// Cats
import cats.Monad
import cats.data.NonEmptyList
import cats.syntax.all._

// Jackson
import com.fasterxml.jackson.databind.ObjectMapper

// LruMap
import com.snowplowanalytics.lrumap.{CreateLruMap, LruMap}

import com.snowplowanalytics.iglu.core.SchemaKey

// JSON Schema
import com.networknt.schema._
import com.networknt.schema.PathType

// circe
import io.circe.Json
import io.circe.jackson.snowplow.circeToJackson

object CirceValidator extends Validator[Json] {

  // SchemaLoader that returns empty JSON for any external reference
  // This prevents the validator from making network calls
  private val noOpSchemaLoader = new SchemaLoader {
    override def getSchema(iri: AbsoluteIri): InputStreamSource = {
      // Return empty JSON object which matches any data
      val emptyJsonObject = "{}"
      val bytes           = emptyJsonObject.getBytes(StandardCharsets.UTF_8)
      () => new ByteArrayInputStream(bytes)
    }
  }

  // These constructors are non-RT because of logging
  private val IgluMetaschema = JsonMetaSchema
    .builder(
      "http://iglucentral.com/schemas/com.snowplowanalytics.self-desc/schema/jsonschema/1-0-0#",
      JsonMetaSchema.getV4
    )
    .keyword(new NonValidationKeyword("self"))
    .build()

  private val V4SchemaInstance = JsonSchemaFactory.getInstance(SpecVersion.VersionFlag.V4)

  private val IgluMetaschemaFactory =
    JsonSchemaFactory
      .builder(V4SchemaInstance)
      .metaSchema(IgluMetaschema)
      .schemaLoaders { loaders => loaders.add(noOpSchemaLoader); () }
      .build()

  private val ValidatorsConfig: SchemaValidatorsConfig =
    SchemaValidatorsConfig
      .builder()
      .pathType(PathType.LEGACY) // Use LEGACY format to match 1.0.76 behavior exactly
      .typeLoose(false) // typeLoose is OpenAPI workaround to cast stringly typed properties
      // e.g, with default true "5" string would validate against integer type
      .build()

  private lazy val V4Schema =
    V4SchemaInstance.getSchema(new ObjectMapper().readTree(MetaSchemas.JsonSchemaV4Text))

  def validate(data: Json, schema: Json): Either[ValidatorError, Unit] =
    for {
      jacksonJson <- circeToJackson(schema, Int.MaxValue).leftMap(_.toInvalidSchema)
      schema      <- evaluateSchema(jacksonJson)
      _           <- validateOnReadySchema(schema, data, Int.MaxValue)
    } yield ()

  @deprecated("Use `checkSchema(schema, maxJsonDepth)`", "3.2.0")
  def checkSchema(schema: Json): List[ValidatorError.SchemaIssue] =
    checkSchema(schema, Int.MaxValue)

  def checkSchema(schema: Json, maxJsonDepth: Int): List[ValidatorError.SchemaIssue] = {
    circeToJackson(schema, maxJsonDepth) match {
      case Left(e)            => List(e.toSchemaIssue)
      case Right(jacksonJson) => validateSchemaAgainstV4(jacksonJson)
    }
  }

  /** Validate instance against schema and return same instance */
  private def validateOnReadySchema(
    schema: JsonSchema,
    instance: Json,
    maxJsonDepth: Int
  ): Either[ValidatorError.InvalidData, Unit] =
    for {
      jacksonJson <- circeToJackson(instance, maxJsonDepth).leftMap(_.toInvalidData)
      _ <- schema.validate(jacksonJson).asScala.toList.map(fromValidationMessage) match {
        case x :: xs => ValidatorError.InvalidData(NonEmptyList(x, xs)).asLeft
        case Nil     => ().asRight
      }
    } yield ()

  private def fromValidationMessage(m: ValidationMessage): ValidatorReport =
    ValidatorReport(
      m.getMessage,
      Option(m.getInstanceLocation()).map(_.toString),
      m.getArguments.toList.map(_.toString),
      m.getType.some
    )

  private def evaluateSchema(
    schemaAsNode: JsonNode
  ): Either[ValidatorError.InvalidSchema, JsonSchema] = {
    Either
      .catchNonFatal(
        IgluMetaschemaFactory
          .getSchema(schemaAsNode, ValidatorsConfig)
      )
      .leftMap(ValidatorError.schemaIssue)
  }

  private def validateSchemaAgainstV4(schema: JsonNode): List[ValidatorError.SchemaIssue] = {
    V4Schema
      .validate(schema)
      .asScala
      .toList
      .map(m =>
        ValidatorError
          .SchemaIssue(Option(m.getInstanceLocation()).map(_.toString).getOrElse(""), m.getMessage)
      )
  }

  private[client] object WithCaching {

    /**
     * Evaluated schema in cache is identified by a schema key and a timestamp indicating when schema was cached by resolver during lookup.
     * Compound key with timestamps allows keeping validator's schema evaluation and resolver's lookup cache in sync.
     * See more in https://github.com/snowplow/iglu-scala-client/issues/207
     */

    type SchemaEvaluationKey         = (SchemaKey, StorageTime)
    type SchemaEvaluationResult      = Either[ValidatorError.InvalidSchema, JsonSchema]
    type SchemaEvaluationCache[F[_]] = LruMap[F, SchemaEvaluationKey, SchemaEvaluationResult]
    type InitValidatorCache[F[_]]    = CreateLruMap[F, SchemaEvaluationKey, SchemaEvaluationResult]

    def validate[F[_]: Monad](
      schemaEvaluationCache: SchemaEvaluationCache[F]
    )(
      data: Json,
      schema: SchemaLookupResult,
      maxJsonDepth: Int
    ): F[Either[ValidatorError, Unit]] = {
      getFromCacheOrEvaluate(schemaEvaluationCache)(schema, maxJsonDepth)
        .map {
          _.flatMap { jsonschema =>
            validateOnReadySchema(jsonschema, data, maxJsonDepth)
          }
        }
    }

    private def getFromCacheOrEvaluate[F[_]: Monad](
      evaluationCache: SchemaEvaluationCache[F]
    )(
      result: SchemaLookupResult,
      maxJsonDepth: Int
    ): F[Either[ValidatorError.InvalidSchema, JsonSchema]] = {
      result match {
        case ResolverResult.Cached(key, SchemaItem(schema, _), timestamp) =>
          evaluationCache.get((key, timestamp)).flatMap {
            case Some(alreadyEvaluatedSchema) =>
              alreadyEvaluatedSchema.pure[F]
            case None =>
              provideNewJsonSchema(schema, maxJsonDepth)
                .pure[F]
                .flatTap(result => evaluationCache.put((key, timestamp), result))
          }
        case ResolverResult.NotCached(SchemaItem(schema, _)) =>
          provideNewJsonSchema(schema, maxJsonDepth).pure[F]
      }
    }

    private def provideNewJsonSchema(
      schema: Json,
      maxJsonDepth: Int
    ): Either[ValidatorError.InvalidSchema, JsonSchema] = {
      for {
        schemaAsNode <- circeToJackson(schema, maxJsonDepth).leftMap(_.toInvalidSchema)
        _            <- validateSchema(schemaAsNode)
        evaluated    <- evaluateSchema(schemaAsNode)
      } yield evaluated
    }

    private def validateSchema(schema: JsonNode): Either[ValidatorError.InvalidSchema, Unit] = {
      val issues = V4Schema
        .validate(schema)
        .asScala
        .toList
        .map(m =>
          ValidatorError.SchemaIssue(
            Option(m.getInstanceLocation()).map(_.toString).getOrElse(""),
            m.getMessage
          )
        )

      issues match {
        case Nil          => Right(())
        case head :: tail => Left(ValidatorError.InvalidSchema(NonEmptyList(head, tail)))
      }
    }
  }
}
