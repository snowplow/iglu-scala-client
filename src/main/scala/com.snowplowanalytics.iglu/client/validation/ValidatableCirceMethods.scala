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
package validation

// Scala
import scala.collection.JavaConverters._

// Cats
import cats.instances.all._
import cats.syntax.all._
import cats.data.{EitherT, NonEmptyList}

import cats.effect.Sync

// circe
import io.circe.Json
import io.circe.optics.JsonPath._

// JSON Schema
import com.networknt.schema._

// Iglu Core
import com.snowplowanalytics.iglu.core.{SchemaCriterion, SchemaKey, SchemaVer}

// This project
import utils.JacksonCatsUtils.circeToJackson
import utils.SchemaKeyUtils

object ValidatableCirceMethods extends Validatable[Json] {

  private val metaSchema = JsonMetaSchema
    .builder(
      "http://iglucentral.com/schemas/com.snowplowanalytics.self-desc/schema/jsonschema/1-0-0#",
      JsonMetaSchema.getDraftV4)
    .build()

  private val factory =
    JsonSchemaFactory.builder(JsonSchemaFactory.getInstance).addMetaSchema(metaSchema).build()

  private def validateOnReadySchema(
    schema: JsonSchema,
    instance: Json): Either[NonEmptyList[ProcessingMessage], Json] = {
    val messages = schema
      .validate(circeToJackson(instance))
      .asScala
      .toList
      .map(message => ProcessingMessage(message.getMessage))

    messages match {
      case x :: xs => NonEmptyList(x, xs).asLeft
      case Nil     => instance.asRight
    }
  }

  def validateAgainstSchema(
    instance: Json,
    schemaJson: Json): Either[NonEmptyList[ProcessingMessage], Json] = {
    Either
      .catchNonFatal(factory.getSchema(circeToJackson(schemaJson)))
      .leftMap(error => NonEmptyList.one(ProcessingMessage(error.getMessage)))
      .flatMap(schema => validateOnReadySchema(schema, instance))
  }

  def validateAndIdentifySchema[F[_]: Sync](
    resolver: Resolver[F],
    instance: Json,
    dataOnly: Boolean = false): F[Either[NonEmptyList[ProcessingMessage], (SchemaKey, Json)]] =
    validateAsSelfDescribing(resolver, instance).productR(
      validateInstance(resolver, instance, dataOnly))

  private def validateInstance[F[_]: Sync](
    resolver: Resolver[F],
    instance: Json,
    dataOnly: Boolean): F[Either[NonEmptyList[ProcessingMessage], (SchemaKey, Json)]] = {
    splitJson(instance)
      .leftMap(NonEmptyList.one)
      .flatTraverse {
        case (key, data) =>
          resolver
            .lookupSchema(key)
            .map(_.flatMap(schema => validateAgainstSchema(data, schema)))
            .map(_.map(_ => if (dataOnly) (key, data) else (key, instance)))
      }
  }

  def verifySchemaAndValidate[F[_]: Sync](
    resolver: Resolver[F],
    instance: Json,
    schemaCriterion: SchemaCriterion,
    dataOnly: Boolean = false): F[Either[NonEmptyList[ProcessingMessage], Json]] = {
    val result = for {
      json         <- EitherT(validateAsSelfDescribing(resolver, instance))
      keyDataTuple <- EitherT.fromEither[F](splitJson(json).leftMap(NonEmptyList.one))
      (key, data) = keyDataTuple

      _ <- EitherT.fromEither[F](
        Either.cond(
          schemaCriterion.matches(key),
          key,
          NonEmptyList.one(ProcessingMessage(
            s"Verifying schema as ${schemaCriterion.asString} failed: found ${key.toSchemaUri}"))
        ))
      schema <- EitherT(resolver.lookupSchema(key))
      _      <- EitherT.fromEither[F](validateAgainstSchema(data, schema))
    } yield if (dataOnly) data else instance

    result.value
  }

  /**
   * Get our schema for self-describing Iglu instances.
   *
   * Unsafe lookup is fine here because we know this
   * schema exists in our resources folder
   */
  private[validation] def getSelfDescribingSchema[F[_]: Sync](
    resolver: Resolver[F]): F[Either[NonEmptyList[ProcessingMessage], Json]] =
    resolver.lookupSchema(
      SchemaKey(
        "com.snowplowanalytics.self-desc",
        "instance-iglu-only",
        "jsonschema",
        SchemaVer.Full(1, 0, 0))
    )

  private def splitJson(json: Json): Either[ProcessingMessage, (SchemaKey, Json)] = {
    val keyOpt  = root.schema.string.getOption(json)
    val dataOpt = root.data.json.getOption(json)

    (keyOpt, dataOpt).tupled
      .toRight(ProcessingMessage(s"Malformed JSON: ${json.spaces2}"))
      .flatMap {
        case (keyString, data) =>
          SchemaKeyUtils.parse(keyString).map(key => (key, data))
      }
  }

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
  private[validation] def validateAsSelfDescribing[F[_]: Sync](
    resolver: Resolver[F],
    instance: Json): F[Either[NonEmptyList[ProcessingMessage], Json]] =
    getSelfDescribingSchema(resolver)
      .map(_.flatMap(selfDesc => validateAgainstSchema(instance, selfDesc)))
}
