/*
 * Copyright (c) 2014-2019 Snowplow Analytics Ltd. All rights reserved.
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

// circe
import io.circe.Json

// LRU
import com.snowplowanalytics.lrumap.CreateLruMap

// Iglu Core
import com.snowplowanalytics.iglu.core.{SchemaKey, SchemaList}

// This project
import resolver.registries.Registry

package object resolver {

  /**
   * Map of all repositories to its aggregated state of failure
   * None as value means repository already responded with `not-found`,
   * meaning all previous 500-like failures could probably been discarded
   */
  type LookupFailureMap = Map[Registry, LookupHistory]

  /**
   * Validated schema lookup result containing, cache result which is
   * Json in case of success or Map of all currently failed repositories
   * in case of failure
   */
  type SchemaLookup = Either[LookupFailureMap, Json]

  type ListLookup = Either[LookupFailureMap, SchemaList]

  /** Schema lookup result associated with timestamp (in seconds) it was stored at */
  type SchemaLookupStamped = (Int, SchemaLookup)

  /** Ability to initialize the cache */
  type InitSchemaCache[F[_]] = CreateLruMap[F, SchemaKey, SchemaLookupStamped]
  type InitListCache[F[_]]   = CreateLruMap[F, (String, String), ListLookup]
}
