/*
 * Copyright (c) 2018-2019 Snowplow Analytics Ltd. All rights reserved.
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
package com.snowplowanalytics.iglu.client.resolver

// Cats
import cats.{Applicative, Functor, Monad}
import cats.data.OptionT
import cats.effect.Clock
import cats.instances.map._
import cats.syntax.semigroup._
import cats.syntax.either._
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.syntax.option._

// LruMap
import com.snowplowanalytics.lrumap.{CreateLruMap, LruMap}

// Iglu core
import com.snowplowanalytics.iglu.core.SchemaKey

/**
 * Resolver cache and associated logic to (in)validate entities,
 * based on TTL and registry responses (failure/success)
 * @param schemas LRUMap with schemas
 * @param schemaLists LRUMap with schema lists
 * @param ttl optional TTL in seconds
 */
class ResolverCache[F[_]] private (
  schemas: LruMap[F, SchemaKey, SchemaLookupStamped],
  schemaLists: LruMap[F, (String, String), ListLookup],
  val ttl: Option[Int]) {

  /**
   * Looks up the given schema key in the cache, respecting TTL
   *
   * @param key The SchemaKey uniquely identifying
   *        the schema in Iglu
   * @return the schema if found as Some Json or None
   *         if not found, or cache is not enabled.
   */
  def getSchema(key: SchemaKey)(implicit F: Monad[F], C: Clock[F]): F[Option[SchemaLookup]] = {
    val wrapped = for {
      (timestamp, lookup) <- OptionT(schemas.get(key))
      seconds             <- OptionT.liftF(currentSeconds)
      if isViable(seconds, timestamp) || lookup.isLeft  // isLeft
    } yield lookup

    wrapped.value
  }

  /**
   * Caches and returns the given schema.
   * If new value is a failure, but cached is success - return cached value in order
   * to avoid invalidating entity due registry outage
   * If new value is a failure, but cached is success -  update TTL in order
   * to avoid flooding poorly behaving registry
   * Also responsible for combining failures
   *
   * @param schemaKey iglu URI that has been requested
   * @param freshResult response from registries, either failure details or schema
   * @return the same result or cached one if it was more appropriate
   */
  def putSchema(schemaKey: SchemaKey, freshResult: SchemaLookup)(
    implicit
    C: Clock[F],
    F: Monad[F]): F[SchemaLookup] =
    for {
      seconds <- currentSeconds
      cached  <- schemas.get(schemaKey) // Ignore TTL invalidation
      result = chooseResult(cached.map(_._2), freshResult)
      _ <- schemas.put(schemaKey, (seconds, result)) // Overwrite or bump TTL
    } yield result

  /** Lookup a `SchemaList`, no TTL is available */
  def getSchemaList(vendor: String, name: String)(implicit F: Monad[F]): F[Option[ListLookup]] =
    schemaLists.get((vendor, name))

  /** Put a `SchemaList` result into a cache */
  def putSchemaList(vendor: String, name: String, list: ListLookup)(
    implicit F: Monad[F]): F[ListLookup] =
    schemaLists.put((vendor, name), list).as(list)

  /** Get Unix epoch timestamp in seconds */
  private def currentSeconds(implicit C: Clock[F], F: Functor[F]): F[Int] =
    C.realTime(java.util.concurrent.TimeUnit.SECONDS).map(_.toInt)

  /** Choose which result is more appropriate for being stored in cache or combine failures */
  private def chooseResult(
    cachedResult: Option[SchemaLookup],
    newResult: SchemaLookup): SchemaLookup =
    (cachedResult, newResult) match {
      case (Some(c), n) if c == n    => c
      case (Some(Left(c)), Left(n))  => c.combine(n).asLeft
      case (Some(Right(c)), Left(_)) => c.asRight
      case _                         => newResult
    }

  /**
   * Check if cached value is still valid based on resolver's `cacheTtl`
   *
   * @param storedTstamp timestamp when value was saved
   * @return true if
   */
  private def isViable(unixSeconds: Int, storedTstamp: Int): Boolean =
    ttl.fold(true)(ttlVal => unixSeconds - storedTstamp < ttlVal)
}

object ResolverCache {
  def init[F[_]: Monad](size: Int, ttl: Option[Int])(
    implicit C: CreateLruMap[F, SchemaKey, SchemaLookupStamped],
    L: CreateLruMap[F, (String, String), ListLookup]): F[Option[ResolverCache[F]]] =
    Option(())
      .filter(_ => size > 0)
      .filter(_ => !ttl.exists(_ <= 0)) match {
      case Some(_) =>
        for {
          schemas     <- CreateLruMap[F, SchemaKey, SchemaLookupStamped].create(size)
          schemaLists <- CreateLruMap[F, (String, String), ListLookup].create(size)
        } yield new ResolverCache[F](schemas, schemaLists, ttl).some
      case None =>
        Applicative[F].pure(none)
    }
}
