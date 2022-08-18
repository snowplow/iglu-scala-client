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

import cats.Monad
import cats.data.EitherT
import cats.effect.Clock
import com.snowplowanalytics.iglu.client.resolver.registries.RegistryLookup
import com.snowplowanalytics.iglu.client.resolver.{InitListCache, InitSchemaCache}
import com.snowplowanalytics.iglu.client.validator.CirceValidator.WithCaching.{
  InitValidatorCache,
  SchemaEvaluationCache,
  SchemaEvaluationKey,
  SchemaEvaluationResult
}
import com.snowplowanalytics.iglu.core.SelfDescribingData
import com.snowplowanalytics.lrumap.CreateLruMap
import io.circe.{DecodingFailure, Json}

final class CirceClient[F[_]] private (
  resolver: Resolver[F],
  schemaEvaluationCache: SchemaEvaluationCache[F]
) {
  def check(
    instance: SelfDescribingData[Json] //TODO can we commit to Json here and get rid of type param?
  )(implicit
    M: Monad[F], //TODO can we move those to client constructor?
    L: RegistryLookup[F],
    C: Clock[F]
  ): EitherT[F, ClientError, Unit] =
    for {
      resolverResult <- EitherT(resolver.lookupSchemaResult(instance.schema))
      validation =
        CirceValidator.WithCaching.validate(schemaEvaluationCache)(instance.data, resolverResult)
      _ <- EitherT(validation).leftMap(_.toClientError)
    } yield ()
}

object CirceClient {

  def parseDefault[F[_]: Monad: InitSchemaCache: InitListCache: InitValidatorCache](
    json: Json
  ): EitherT[F, DecodingFailure, CirceClient[F]] =
    for {
      resolver <- EitherT(Resolver.parse[F](json))
      cache    <- EitherT.liftF(schemaEvaluationCache[F])
    } yield new CirceClient(resolver, cache)

  private def schemaEvaluationCache[F[_]: InitValidatorCache]: F[SchemaEvaluationCache[F]] =
    CreateLruMap[F, SchemaEvaluationKey, SchemaEvaluationResult].create(100)

}
