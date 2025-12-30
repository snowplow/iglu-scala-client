/*
 * Copyright (c) 2018-2023 Snowplow Analytics Ltd. All rights reserved.
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
import com.snowplowanalytics.lrumap.LruMap

// Iglu core
import com.snowplowanalytics.iglu.core.{SchemaKey, SchemaList}

import com.snowplowanalytics.iglu.client.resolver.registries.RegistryError

import Resolver.SchemaItem

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
  schemaContentLists: LruMap[F, SchemaKey, SchemaContentListCacheEntry],
  schemaMutex: ResolverMutex[F, SchemaKey],
  schemaListMutex: ResolverMutex[F, ListCacheKey],
  schemaContentListMutex: ResolverMutex[F, SchemaKey],
  val ttl: Option[TTL]
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

  private[resolver] def getTimestampedSchema(
    key: SchemaKey
  )(implicit F: Monad[F], C: Clock[F]): F[Option[TimestampedItem[SchemaLookup]]] =
    getTimestampedItem(ttl, schemas, key)

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

  /**
   * Much like `putSchema` but carries information about whether the cache was updated as a
   * consequence, and the timestamp associated with the cached schema.
   *
   * Scoped as private because this method is new, somewhat experimental, and currently not needed
   * beyond the internals of this lib.
   */
  private[resolver] def putSchemaResult(
    schemaKey: SchemaKey,
    freshResult: SchemaLookup
  )(implicit
    F: Monad[F],
    C: Clock[F]
  ): F[Either[LookupFailureMap, TimestampedItem[SchemaItem]]] =
    putItemResult(schemas, schemaKey, freshResult)

  /** Lookup a `SchemaList`, no TTL is available */
  def getSchemaList(
    vendor: Vendor,
    name: Name,
    model: Model
  )(implicit
    F: Monad[F],
    C: Clock[F]
  ): F[Option[ListLookup]] =
    getItem(ttl, schemaLists, (vendor, name, model))

  private[resolver] def getTimestampedSchemaList(vendor: Vendor, name: Name, model: Model)(implicit
    F: Monad[F],
    C: Clock[F]
  ): F[Option[TimestampedItem[ListLookup]]] =
    getTimestampedItem(ttl, schemaLists, (vendor, name, model))

  /** Put a `SchemaList` result into a cache */
  def putSchemaList(
    vendor: Vendor,
    name: Name,
    model: Model,
    list: ListLookup
  )(implicit
    F: Monad[F],
    C: Clock[F]
  ): F[ListLookup] =
    putItem(schemaLists, (vendor, name, model), list)

  private[resolver] def putSchemaListResult(
    vendor: Vendor,
    name: Name,
    model: Model,
    freshResult: ListLookup
  )(implicit F: Monad[F], C: Clock[F]): F[Either[LookupFailureMap, TimestampedItem[SchemaList]]] =
    putItemResult(schemaLists, (vendor, name, model), freshResult)

  private[resolver] def withLockOnSchemaKey[A](key: SchemaKey)(f: => F[A]): F[A] =
    schemaMutex.withLockOn(key)(f)

  private[resolver] def withLockOnSchemaModel[A](vendor: Vendor, name: Name, model: Model)(
    f: => F[A]
  ): F[A] =
    schemaListMutex.withLockOn((vendor, name, model))(f)

  private[resolver] def getTimestampedSchemaContentList(
    schemaKey: SchemaKey
  )(implicit F: Monad[F], C: Clock[F]): F[Option[TimestampedItem[SchemaContentListLookup]]] =
    getTimestampedItem(ttl, schemaContentLists, schemaKey)

  private[resolver] def putSchemaContentListResult(
    schemaKey: SchemaKey,
    schemas: SchemaContentListLookup
  )(implicit
    F: Monad[F],
    C: Clock[F]
  ): F[Either[LookupFailureMap, TimestampedItem[SchemaContentList]]] =
    putItemResult(schemaContentLists, schemaKey, schemas)

  private[resolver] def withLockOnSchemaContentList[A](key: SchemaKey)(f: => F[A]): F[A] =
    schemaContentListMutex.withLockOn(key)(f)
}

object ResolverCache {

  type Lookup[A]   = Either[LookupFailureMap, A]
  type CurrentTime = FiniteDuration

  def init[F[_]: Monad](
    size: Int,
    ttl: Option[TTL]
  )(implicit
    C: CreateResolverCache[F]
  ): F[Option[ResolverCache[F]]] = {
    if (shouldCreateResolverCache(size, ttl)) {
      for {
        schemas                <- C.createSchemaCache(size)
        schemaLists            <- C.createSchemaListCache(size)
        schemaContentLists     <- C.createSchemaContentListCache(size)
        schemaMutex            <- C.createMutex[SchemaKey]
        listMutex              <- C.createMutex[ListCacheKey]
        schemaContentListMutex <- C.createMutex[SchemaKey]
      } yield new ResolverCache[F](
        schemas,
        schemaLists,
        schemaContentLists,
        schemaMutex,
        listMutex,
        schemaContentListMutex,
        ttl
      ).some
    } else
      Applicative[F].pure(none)
  }

  private[resolver] def shouldCreateResolverCache(size: Int, ttl: Option[TTL]): Boolean =
    size > 0 && ttl.forall(_ > 0.seconds)

  private def getTimestampedItem[F[_]: Monad: Clock, A, K](
    ttl: Option[TTL],
    c: LruMap[F, K, CacheEntry[Lookup[A]]],
    key: K
  ): F[Option[TimestampedItem[Lookup[A]]]] =
    OptionT(c.get(key))
      .semiflatMap[Option[TimestampedItem[Lookup[A]]]] { case (storageTime, lookup) =>
        Clock[F].realTime.map { currentTime =>
          if (isViable(ttl, currentTime, storageTime))
            Some(TimestampedItem(lookup, storageTime))
          else {
            lookup match {
              case Right(_) => None
              case Left(failures) =>
                val noNotFounds = failures.map { case (k, history) =>
                  k -> history.copy(errors = history.errors - RegistryError.NotFound)
                }
                Some(TimestampedItem(Left(noNotFounds), storageTime))
            }
          }
        }
      }
      .subflatMap { case o: Option[TimestampedItem[Lookup[A]]] => o }
      .value

  def getItem[F[_]: Monad: Clock, A, K](
    ttl: Option[TTL],
    c: LruMap[F, K, CacheEntry[Lookup[A]]],
    key: K
  ): F[Option[Lookup[A]]] =
    getTimestampedItem(ttl, c, key).map(_.map(_.value))

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
    c: LruMap[F, K, CacheEntry[Lookup[A]]],
    schemaKey: K,
    freshResult: Lookup[A]
  ): F[Lookup[A]] = putItemResult(c, schemaKey, freshResult).map(_.map(_.value))

  /**
   * Much like `putItem` but carries information about whether the cache was updated as a
   * consequence, and the timestamp associated with the cached item.
   */
  private def putItemResult[F[_]: Monad: Clock, A, K](
    c: LruMap[F, K, CacheEntry[Lookup[A]]],
    schemaKey: K,
    freshResult: Lookup[A]
  ): F[Either[LookupFailureMap, TimestampedItem[A]]] =
    for {
      seconds <- Clock[F].realTime
      cached  <- c.get(schemaKey) // Ignore TTL invalidation
      result = chooseResult(cached.map(_._2), freshResult, seconds)
      _ <- c.put(schemaKey, (seconds, result.map(_.value))) // Overwrite or bump TTL
    } yield result

  /**
   * Check if cached value is still valid based on resolver's `cacheTtl`
   *
   * @param storageTime time when value was saved
   * @return true if
   */
  private def isViable(
    ttl: Option[TTL],
    currentTime: CurrentTime,
    storageTime: StorageTime
  ): Boolean =
    ttl match {
      case Some(definedTtl) => currentTime - storageTime < definedTtl
      case None             => true
    }

  /** Choose which result is more appropriate for being stored in cache or combine failures */
  private def chooseResult[A](
    cachedResult: Option[Lookup[A]],
    newResult: Lookup[A],
    newSeconds: TTL
  ): Either[LookupFailureMap, TimestampedItem[A]] = (cachedResult, newResult) match {
    case (Some(Right(c)), Right(n)) if c == n =>
      Right(
        TimestampedItem(c, newSeconds)
      ) // if the cached objects are the same, update the timestamp
    case (Some(Right(_)), Right(n)) =>
      Right(TimestampedItem(n, newSeconds)) // update w new item and update the timestamp
    case (Some(Left(c)), Left(n)) =>
      Left(c.combine(n)) // if both cache and new item are actually an error
    case (Some(Right(c)), Left(_)) => Right(TimestampedItem(c, newSeconds))
    case (None, Left(n))           => Left(n)
    case (None, Right(n))          => Right(TimestampedItem(n, newSeconds))
    case (Some(Left(_)), Right(n)) => Right(TimestampedItem(n, newSeconds))
  }

  /**
   * The result of committing a lookup to the cache
   *
   * @param value the cached value
   * @param timestamp the epoch time in seconds for when this value was originally cached
   */
  private[resolver] case class TimestampedItem[A](value: A, timestamp: StorageTime)
}
