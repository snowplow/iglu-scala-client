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
import cats.effect.{Clock, IO}

import io.circe.{DecodingFailure, Json}

import com.snowplowanalytics.iglu.core.SelfDescribingData

import resolver.{InitListCache, InitSchemaCache}
import resolver.registries.{Registry, RegistryLookup}
import validator.CirceValidator.InitValidatorCache

/**
 * Umbrella entity, able to perform all necessary actions:
 * - resolve schema
 * - validate schema
 * - validate datum against the schema
 *
 * Unlike the [[Client]] class, a Client2 is backed by a [[ValidatorF]], which uses caching to
 * allow more efficient schema checking.
 */

final case class Client2[F[_], A](
  resolver: Resolver[F],
  validator: ValidatorF[F, A]
) {
  def check(
    instance: SelfDescribingData[A]
  )(implicit
    M: Monad[F],
    L: RegistryLookup[F],
    C: Clock[F]
  ): EitherT[F, ClientError, Unit] =
    for {
      resolverResult <- EitherT(resolver.lookupSchemaResult(instance.schema))
      schemaValidation = validator.validateSchemaF(resolverResult)
      _ <- EitherT(schemaValidation).leftMap(_.toClientError)
      validation = validator.validateF(instance.data, resolverResult)
      _ <- EitherT(validation).leftMap(_.toClientError)
    } yield ()
}

object Client2 {

  /** Default Iglu Central client, without cache */
  val igluCentral: IO[Client2[IO, Json]] =
    for {
      validator <- CirceValidator.validatorF[IO]
    } yield Client2[IO, Json](Resolver(List(Registry.IgluCentral), None), validator)

  def parseDefault[F[_]: Monad: InitSchemaCache: InitListCache: InitValidatorCache](
    json: Json
  ): EitherT[F, DecodingFailure, Client2[F, Json]] =
    for {
      resolver  <- EitherT(Resolver.parse[F](json))
      validator <- EitherT.liftF(CirceValidator.validatorF[F])
    } yield Client2(resolver, validator)

}
