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

/**
 * Umbrella entity, able to perform all necessary actions:
 * - resolve schema
 * - validate schema
 * - validate datum against the schema
 *
 * Almost identical to pre-0.6.0 resolver
 */
final case class Client[F[_], A](resolver: Resolver[F], validator: Validator[A]) {
  def check(
    instance: SelfDescribingData[A]
  )(implicit
    M: Monad[F],
    L: RegistryLookup[F],
    C: Clock[F]
  ): EitherT[F, ClientError, Unit] =
    for {
      schema <- EitherT(resolver.lookupSchema(instance.schema))
      schemaValidation = validator.validateSchema(schema)
      _ <- EitherT.fromEither[F](schemaValidation).leftMap(_.toClientError(None))
      validation = validator.validate(instance.data, schema)
      _ <- EitherT.fromEither[F](validation).leftMap(_.toClientError(None))
    } yield ()
}

object Client {

  /** Default Iglu Central client, without cache */
  val IgluCentral: Client[IO, Json] =
    Client[IO, Json](Resolver(List(Registry.IgluCentral), None), CirceValidator)

  def parseDefault[F[_]: Monad: InitSchemaCache: InitListCache](
    json: Json
  ): EitherT[F, DecodingFailure, Client[F, Json]] =
    EitherT(Resolver.parse[F](json)).map { resolver =>
      Client(resolver, CirceValidator)
    }
}
