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
import cats.implicits._

import io.circe.{DecodingFailure, Json}

import com.snowplowanalytics.iglu.core.{SchemaKey, SelfDescribingData}

import resolver.{InitListCache, InitSchemaCache}
import resolver.registries.{Registry, RegistryLookup}
import validator.CirceValidator.InitValidatorCache

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
      _ <- EitherT.fromEither[F](schemaValidation).leftMap(_.toClientError)
      validation = validator.validate(instance.data, schema)
      _ <- EitherT.fromEither[F](validation).leftMap(_.toClientError)
    } yield ()
}

object Client {

  /** Default Iglu Central client, without cache */
  val IgluCentral: Client[IO, Json] =
    Client[IO, Json](Resolver(List(Registry.IgluCentral), None), CirceValidator)

  def parseDefault[F[_]: Monad: InitSchemaCache: InitListCache](json: Json) =
    EitherT(Resolver.parse[F](json)).map { resolver =>
      Client(resolver, CirceValidator)
    }
}

final case class Client2[F[_], A](
  resolver: Resolver[F],
  validator: ValidatorF[F, (SchemaKey, Int), A]
) {
  def check(
    instance: SelfDescribingData[A]
  )(implicit
    M: Monad[F],
    L: RegistryLookup[F],
    C: Clock[F]
  ): EitherT[F, ClientError, Unit] =
    for {
      (schema, tstamp) <- EitherT(resolver.lookupSchemaResult(instance.schema).map(_.toEither))
      key              = tstamp.map((instance.schema, _))
      schemaValidation = validator.validateSchemaF(key, schema)
      _ <- EitherT(schemaValidation).leftMap(_.toClientError)
      validation = validator.validateF(key, instance.data, schema)
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
