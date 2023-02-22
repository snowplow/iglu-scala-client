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
import cats.effect.std.Mutex
import cats.effect.Async
import cats.implicits._
import com.snowplowanalytics.lrumap.LruMap

trait ResolverMutex[F[_], K] {

  def withLockOn[A](key: K)(f: => F[A]): F[A]

}

object ResolverMutex {

  private[resolver] def idResolverMutex[K](
    topMutex: Object,
    keyedMutex: LruMap[Id, K, Object]
  ): ResolverMutex[Id, K] =
    new ResolverMutex[Id, K] {
      def withLockOn[A](key: K)(f: => A): A = {
        val mutexForKey = topMutex.synchronized {
          val current = keyedMutex.get(key)
          current match {
            case Some(o) =>
              o
            case None =>
              val o = new Object
              keyedMutex.put(key, o)
              o
          }
        }

        mutexForKey.synchronized(f)
      }
    }

  private[resolver] def asyncResolverMutex[F[_]: Async, K](
    topMutex: Mutex[F],
    keyedMutex: LruMap[F, K, Mutex[F]]
  ): ResolverMutex[F, K] =
    new ResolverMutex[F, K] {

      def withLockOn[A](key: K)(f: => F[A]): F[A] =
        topMutex.lock
          .surround {
            for {
              current <- keyedMutex.get(key)
              fixed <- current match {
                case Some(m) => Async[F].pure(m)
                case None =>
                  for {
                    m <- Mutex[F]
                    _ <- keyedMutex.put(key, m)
                  } yield m
              }
            } yield fixed
          }
          .flatMap { mutexForKey =>
            mutexForKey.lock.surround(f)
          }
    }

}
