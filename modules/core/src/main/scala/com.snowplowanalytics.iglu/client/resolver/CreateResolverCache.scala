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

import cats.Id
import cats.effect.Async
import cats.effect.std.Mutex
import cats.implicits._
import com.snowplowanalytics.lrumap.{CreateLruMap, LruMap}
import com.snowplowanalytics.iglu.core.SchemaKey

trait CreateResolverCache[F[_]] {

  def createSchemaCache(size: Int): F[LruMap[F, SchemaKey, SchemaCacheEntry]]

  def createSchemaListCache(size: Int): F[LruMap[F, ListCacheKey, ListCacheEntry]]

  def createMutex[K]: F[ResolverMutex[F, K]]

}

object CreateResolverCache {

  def apply[F[_]](implicit instance: CreateResolverCache[F]): CreateResolverCache[F] = instance

  private trait SimpleCreateResolverCache[F[_]] extends CreateResolverCache[F] {

    def createLruMap[K, V](size: Int): F[LruMap[F, K, V]]

    override def createSchemaCache(size: Int): F[LruMap[F, SchemaKey, SchemaCacheEntry]] =
      createLruMap(size)

    override def createSchemaListCache(size: Int): F[LruMap[F, ListCacheKey, ListCacheEntry]] =
      createLruMap(size)

  }

  implicit def idCreateResolverCache: CreateResolverCache[Id] =
    new SimpleCreateResolverCache[Id] {
      def createLruMap[K, V](size: Int): LruMap[Id, K, V] =
        CreateLruMap[Id, K, V].create(size)

      def createMutex[K]: ResolverMutex[Id, K] =
        ResolverMutex.idResolverMutex(new Object, createLruMap[K, Object](1000))
    }

  implicit def asyncCreateResolverCache[F[_]: Async]: CreateResolverCache[F] =
    new SimpleCreateResolverCache[F] {

      def createLruMap[K, V](size: Int): F[LruMap[F, K, V]] =
        CreateLruMap[F, K, V].create(size)

      def createMutex[K]: F[ResolverMutex[F, K]] =
        for {
          mutex  <- Mutex[F]
          lrumap <- createLruMap[K, Mutex[F]](1000)
        } yield ResolverMutex.asyncResolverMutex(mutex, lrumap)
    }
}
