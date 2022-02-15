/*
 * Copyright (c) 2018-2021 Snowplow Analytics Ltd. All rights reserved.
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
import cats.{Applicative, Monad}
import cats.data.OptionT
import cats.effect.Clock
import cats.implicits._

import scala.concurrent.duration.{DurationInt, FiniteDuration}

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
  schemas: LruMap[F, SchemaKey, SchemaCacheEntry],
  schemaLists: LruMap[F, ListCacheKey, ListCacheEntry],
  val ttl: Option[FiniteDuration]
) {

  import ResolverCache._

  /**
   * Looks up the given schema key in the cache, respecting TTL
   *
   * @param key The SchemaKey uniquely identifying
   *        the schema in Iglu
   * @return the schema if found as Some Json or None
   *         if not found, or cache is not enabled.
   */
  def getSchema(key: SchemaKey)(implicit F: Monad[F], C: Clock[F]): F[Option[SchemaLookup]] =
    getItem(ttl, schemas, key)

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
  def putSchema(
    schemaKey: SchemaKey,
    freshResult: SchemaLookup
  )(implicit
    F: Monad[F],
    C: Clock[F]
  ): F[SchemaLookup] =
    putItem(schemas, schemaKey, freshResult)

  /** Lookup a `SchemaList`, no TTL is available */
  def getSchemaList(
    vendor: String,
    name: String,
    model: Int
  )(implicit
    F: Monad[F],
    C: Clock[F]
  ): F[Option[ListLookup]] =
    getItem(ttl, schemaLists, (vendor, name, model))

  /** Put a `SchemaList` result into a cache */
  def putSchemaList(
    vendor: String,
    name: String,
    model: Int,
    list: ListLookup
  )(implicit
    F: Monad[F],
    C: Clock[F]
  ): F[ListLookup] =
    putItem(schemaLists, (vendor, name, model), list)
}

object ResolverCache {

  type Lookup[A] = Either[LookupFailureMap, A]

  def init[F[_]: Monad](
    size: Int,
    ttl: Option[FiniteDuration]
  )(implicit
    C: CreateLruMap[F, SchemaKey, SchemaCacheEntry],
    L: CreateLruMap[F, ListCacheKey, ListCacheEntry]
  ): F[Option[ResolverCache[F]]] = {
    if (size >= 0 && ttl.forall(_ > 0.seconds)) {
      for {
        schemas     <- CreateLruMap[F, SchemaKey, SchemaCacheEntry].create(size)
        schemaLists <- CreateLruMap[F, ListCacheKey, ListCacheEntry].create(size)
      } yield new ResolverCache[F](schemas, schemaLists, ttl).some
    } else
      Applicative[F].pure(none)
  }

  def getItem[F[_]: Monad: Clock, A, K](
    ttl: Option[FiniteDuration],
    c: LruMap[F, K, (FiniteDuration, Lookup[A])],
    key: K
  ): F[Option[Lookup[A]]] = {
    val wrapped = for {
      (timestamp, lookup) <- OptionT(c.get(key))
      currentTime         <- OptionT.liftF(Clock[F].realTime)
      _ = currentTime
      if isViable(ttl, currentTime, timestamp) || lookup.isLeft // isLeft
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
  def putItem[F[_]: Monad: Clock, A, K](
    c: LruMap[F, K, (FiniteDuration, Lookup[A])],
    schemaKey: K,
    freshResult: Lookup[A]
  ): F[Lookup[A]] =
    for {
      currentTime <- Clock[F].realTime
      cached      <- c.get(schemaKey) // Ignore TTL invalidation
      result = chooseResult(cached.map(_._2), freshResult)
      _ <- c.put(schemaKey, (currentTime, result)) // Overwrite or bump TTL
    } yield result

  /**
   * Check if cached value is still valid based on resolver's `cacheTtl`
   *
   * @param storedTstamp timestamp when value was saved
   * @return true if
   */
  private def isViable(
    ttl: Option[FiniteDuration],
    currentTime: FiniteDuration,
    storedTstamp: FiniteDuration
  ): Boolean =
    ttl match {
      case Some(definedTtl) => currentTime - storedTstamp < definedTtl
      case None             => true
    }

  /** Choose which result is more appropriate for being stored in cache or combine failures */
  private def chooseResult[A](cachedResult: Option[Lookup[A]], newResult: Lookup[A]): Lookup[A] =
    (cachedResult, newResult) match {
      case (Some(c), n) if c == n    => c
      case (Some(Left(c)), Left(n))  => c.combine(n).asLeft
      case (Some(Right(c)), Left(_)) => c.asRight
      case _                         => newResult
    }
}
