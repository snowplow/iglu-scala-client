/*
 * Copyright (c) 2014-2022 Snowplow Analytics Ltd. All rights reserved.
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

import cats.Monad
import cats.data.EitherT
import cats.effect.Clock
import cats.implicits._
import com.snowplowanalytics.iglu.client.resolver.registries.RegistryLookup
import com.snowplowanalytics.iglu.client.resolver.{InitListCache, InitSchemaCache}
import com.snowplowanalytics.iglu.client.resolver.Resolver.SupersededBy
import com.snowplowanalytics.iglu.client.validator.CirceValidator.WithCaching.{
  InitValidatorCache,
  SchemaEvaluationCache,
  SchemaEvaluationKey,
  SchemaEvaluationResult
}
import com.snowplowanalytics.iglu.core.SelfDescribingData
import com.snowplowanalytics.lrumap.CreateLruMap
import io.circe.{DecodingFailure, Json}

/**
 * Client using 'lookupSchemaResult' resolver method (as opposed to old client relying on plain `lookupSchema` method)
 * which enables validator taking advantage of caching.
 * Should provide significant performance boost for the 'check' operation when called frequently.
 */
final class IgluCirceClient[F[_]] private (
  resolver: Resolver[F],
  schemaEvaluationCache: SchemaEvaluationCache[F]
) {
  def check(
    instance: SelfDescribingData[Json]
  )(implicit
    M: Monad[F],
    L: RegistryLookup[F],
    C: Clock[F]
  ): EitherT[F, ClientError, SupersededBy] =
    for {
      resolverResult <- EitherT(resolver.lookupSchemaResult(instance.schema))
      validation =
        CirceValidator.WithCaching.validate(schemaEvaluationCache)(instance.data, resolverResult)
      _ <- EitherT(validation).leftMap(e =>
        e.toClientError(resolverResult.value.supersededBy.map(_.asString))
      )
      // Returning superseding schema info as well since we want to inform caller that sdj is validated
      // against superseding schema if it is superseded by another schema.
    } yield resolverResult.value.supersededBy
}

object IgluCirceClient {

  def parseDefault[F[_]: Monad: InitSchemaCache: InitListCache: InitValidatorCache](
    json: Json
  ): EitherT[F, DecodingFailure, IgluCirceClient[F]] =
    for {
      config   <- EitherT.fromEither[F](Resolver.parseConfig(json))
      resolver <- Resolver.fromConfig[F](config)
      client   <- EitherT.liftF(fromResolver(resolver, config.cacheSize))
    } yield client

  def fromResolver[F[_]: Monad: InitValidatorCache](
    resolver: Resolver[F],
    cacheSize: Int
  ): F[IgluCirceClient[F]] = {
    schemaEvaluationCache[F](cacheSize).map { cache =>
      new IgluCirceClient(resolver, cache)
    }
  }

  private def schemaEvaluationCache[F[_]: InitValidatorCache](
    cacheSize: Int
  ): F[SchemaEvaluationCache[F]] =
    CreateLruMap[F, SchemaEvaluationKey, SchemaEvaluationResult].create(cacheSize)

}
