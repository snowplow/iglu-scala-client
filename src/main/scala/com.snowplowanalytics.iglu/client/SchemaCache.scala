package com.snowplowanalytics.iglu.client

// Cats
import cats.effect.Sync
import cats.syntax.apply._
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.syntax.monadError._
import cats.data.OptionT

// Iglu core
import com.snowplowanalytics.iglu.core.SchemaKey

// LruMap
import com.snowplowanalytics.lrumap.LruMap

case class SchemaCache[F[_]: Sync] private (
  lru: LruMap[F, SchemaKey, SchemaLookupStamped],
  ttl: Option[Int]) {

  /**
   * Looks up the given schema key in the cache.
   *
   * @param schemaKey The SchemaKey uniquely identifying
   *        the schema in Iglu
   * @return the schema if found as Some Json or None
   *         if not found, or cache is not enabled.
   */
  def get(key: SchemaKey): F[Option[SchemaLookup]] = {
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
  def store(schemaKey: SchemaKey, schema: SchemaLookup): F[SchemaLookup] =
    for {
      seconds <- currentSeconds
      _       <- lru.put(schemaKey, (seconds, schema))
    } yield schema

  /**
   * Get Unix epoch timestamp in seconds
   */
  private val currentSeconds: F[Int] =
    Sync[F].delay((System.currentTimeMillis() / 1000).toInt)

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

  def apply[F[_]: Sync](size: Int, ttl: Option[Int] = None): F[SchemaCache[F]] = {
    Sync[F].unit
      .ensure(new IllegalArgumentException("Cache size must be greater than 0"))(_ => size > 0)
      .ensure(new IllegalArgumentException("Cache TTL must be greater than 0"))(_ =>
        !ttl.exists(_ <= 0))
      .productR(LruMap.create[F, SchemaKey, SchemaLookupStamped](size))
      .map(lru => new SchemaCache[F](lru, ttl))
  }

}
