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

import com.snowplowanalytics.iglu.client.resolver.Resolver.ResolverResult
import com.snowplowanalytics.iglu.client.resolver.StorageTime
import com.snowplowanalytics.iglu.client.resolver.Resolver.{SchemaItem, SchemaLookupResult}

// Cats
import cats.Monad
import cats.data.NonEmptyList
import cats.syntax.all._

// LruMap
import com.snowplowanalytics.lrumap.{CreateLruMap, LruMap}

import com.snowplowanalytics.iglu.core.SchemaKey

// Native JSON Schema Validator
import com.snowplowanalytics.iglu.jsonschema.{ValidationError, Validator => NativeValidator}

// circe
import io.circe.Json

object CirceValidator extends validator.Validator[Json] {

  type CompiledSchema = com.snowplowanalytics.iglu.jsonschema.CompiledSchema

  def validate(data: Json, schema: Json): Either[ValidatorError, Unit] =
    for {
      compiled <- compileSchema(schema, Int.MaxValue)
      _        <- validateOnReadySchema(compiled, data, Int.MaxValue)
    } yield ()

  @deprecated("Use `checkSchema(schema, maxJsonDepth)`", "3.2.0")
  def checkSchema(schema: Json): List[ValidatorError.SchemaIssue] =
    checkSchema(schema, Int.MaxValue)

  def checkSchema(schema: Json, maxJsonDepth: Int): List[ValidatorError.SchemaIssue] = {
    if (maxJsonDepth < Int.MaxValue && jsonDepth(schema) > maxJsonDepth)
      List(ValidatorError.SchemaIssue("/", "Maximum allowed JSON depth exceeded"))
    else
      NativeValidator.checkSchema(schema).toList.map { e =>
        ValidatorError.SchemaIssue(
          e.path.render,
          e.render
        )
      }
  }

  private def validateOnReadySchema(
    compiled: CompiledSchema,
    instance: Json,
    maxJsonDepth: Int
  ): Either[ValidatorError.InvalidData, Unit] = {
    if (maxJsonDepth < Int.MaxValue && jsonDepth(instance) > maxJsonDepth) {
      val report = ValidatorReport(
        "Maximum allowed JSON depth exceeded",
        Some("/"),
        List.empty,
        None
      )
      return ValidatorError.InvalidData(NonEmptyList.one(report)).asLeft
    }
    compiled.validate(instance, maxJsonDepth).toList.map(fromValidationError) match {
      case x :: xs => ValidatorError.InvalidData(NonEmptyList(x, xs)).asLeft
      case Nil     => ().asRight
    }
  }

  private def fromValidationError(e: ValidationError): ValidatorReport = {
    // For required/additionalProperties, message already includes the child path prefix.
    // For all other keywords, we prepend the path.
    val message = e.keyword match {
      case "required" | "additionalProperties" => e.message
      case _                                   => e.render
    }
    ValidatorReport(
      message,
      Some(e.path.render),
      e.targets,
      Some(e.keyword)
    )
  }

  def compileJsonSchema(
    schema: Json,
    maxJsonDepth: Int
  ): Either[ValidatorError.InvalidSchema, CompiledSchema] =
    compileSchema(schema, maxJsonDepth)

  def validateCompiled(
    data: Json,
    compiledSchema: CompiledSchema,
    maxJsonDepth: Int = Int.MaxValue
  ): Either[ValidatorError, Unit] =
    validateOnReadySchema(compiledSchema, data, maxJsonDepth)

  private def compileSchema(
    schema: Json,
    maxJsonDepth: Int
  ): Either[ValidatorError.InvalidSchema, CompiledSchema] = {
    if (maxJsonDepth < Int.MaxValue && jsonDepth(schema) > maxJsonDepth) {
      val issue = ValidatorError.SchemaIssue("/", "Maximum allowed JSON depth exceeded")
      return ValidatorError.InvalidSchema(NonEmptyList.one(issue)).asLeft
    }
    NativeValidator
      .compile(schema)
      .leftMap(err =>
        ValidatorError.InvalidSchema(
          NonEmptyList.one(ValidatorError.SchemaIssue(err.path, err.message))
        )
      )
  }

  private def jsonDepth(json: Json): Int = {
    def go(j: Json, depth: Int): Int =
      j.fold(
        jsonNull = depth,
        jsonBoolean = _ => depth,
        jsonNumber = _ => depth,
        jsonString = _ => depth,
        jsonArray = arr =>
          if (arr.isEmpty) depth
          else arr.foldLeft(depth)((max, elem) => math.max(max, go(elem, depth + 1))),
        jsonObject = obj =>
          if (obj.isEmpty) depth
          else obj.toIterable.foldLeft(depth) { case (max, (_, v)) => math.max(max, go(v, depth + 1)) }
      )
    go(json, 1)
  }

  private[client] object WithCaching {

    type SchemaEvaluationKey         = (SchemaKey, StorageTime)
    type SchemaEvaluationResult      = Either[ValidatorError.InvalidSchema, CompiledSchema]
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
          _.flatMap { compiled =>
            validateOnReadySchema(compiled, data, maxJsonDepth)
          }
        }
    }

    private def getFromCacheOrEvaluate[F[_]: Monad](
      evaluationCache: SchemaEvaluationCache[F]
    )(
      result: SchemaLookupResult,
      maxJsonDepth: Int
    ): F[Either[ValidatorError.InvalidSchema, CompiledSchema]] = {
      result match {
        case ResolverResult.Cached(key, SchemaItem(schema, _), timestamp) =>
          evaluationCache.get((key, timestamp)).flatMap {
            case Some(alreadyEvaluatedSchema) =>
              alreadyEvaluatedSchema.pure[F]
            case None =>
              compileSchema(schema, maxJsonDepth)
                .pure[F]
                .flatTap(result => evaluationCache.put((key, timestamp), result))
          }
        case ResolverResult.NotCached(SchemaItem(schema, _)) =>
          compileSchema(schema, maxJsonDepth).pure[F]
      }
    }

  }
}
