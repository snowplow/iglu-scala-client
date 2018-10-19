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
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.syntax.option._

// LruMap
import com.snowplowanalytics.lrumap.{CreateLruMap, LruMap}

// Iglu core
import com.snowplowanalytics.iglu.core.SchemaKey

class SchemaCache[F[_]] private (
  lru: LruMap[F, SchemaKey, SchemaLookupStamped],
  val ttl: Option[Int]) {

  /**
   * Looks up the given schema key in the cache.
   *
   * @param key The SchemaKey uniquely identifying
   *        the schema in Iglu
   * @return the schema if found as Some Json or None
   *         if not found, or cache is not enabled.
   */
  def get(key: SchemaKey)(implicit F: Monad[F], C: Clock[F]): F[Option[SchemaLookup]] = {
    val wrapped = for {
      (timestamp, lookup) <- OptionT(lru.get(key))
      seconds             <- OptionT.liftF(currentSeconds)
      if isViable(seconds, timestamp)
    } yield lookup

    wrapped.value
  }

  /**
   * Caches and returns the given schema. Does
   * nothing if we don't have an LRU cache
   * available.
   *
   * @param schema The provided schema
   * @return the same schema
   */
  def store(schemaKey: SchemaKey, schema: SchemaLookup)(
    implicit
    C: Clock[F],
    F: Monad[F]): F[SchemaLookup] =
    for {
      seconds <- currentSeconds
      _       <- lru.put(schemaKey, (seconds, schema))
    } yield schema

  /**
   * Get Unix epoch timestamp in seconds
   * TODO: switch to monotonic
   */
  private def currentSeconds(implicit C: Clock[F], F: Functor[F]): F[Int] =
    C.realTime(java.util.concurrent.TimeUnit.SECONDS).map(_.toInt)

  /**
   * Check if cached value is still valid based on resolver's `cacheTtl`
   *
   * @param storedTstamp timestamp when value was saved
   * @return true if
   */
  private def isViable(unixSeconds: Int, storedTstamp: Int): Boolean =
    ttl.fold(true)(ttlVal => unixSeconds - storedTstamp < ttlVal)
}

object SchemaCache {
  def init[F[_]: Applicative: Cache](size: Int, ttl: Option[Int]): F[Option[SchemaCache[F]]] =
    Option(())
      .filter(_ => size > 0)
      .filter(_ => !ttl.exists(_ <= 0)) match {
      case Some(_) =>
        CreateLruMap[F, SchemaKey, SchemaLookupStamped]
          .create(size)
          .map(lru => new SchemaCache[F](lru, ttl).some)
      case None =>
        Applicative[F].pure(none)
    }
}
