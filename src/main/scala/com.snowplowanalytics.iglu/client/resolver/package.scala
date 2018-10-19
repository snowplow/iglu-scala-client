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
import com.snowplowanalytics.iglu.client.ClientError.ResolverError
import io.circe.Json

// LRU
import com.snowplowanalytics.lrumap.{CreateLruMap, LruMap}

// Iglu Core
import com.snowplowanalytics.iglu.core.SchemaKey

// This project
import resolver.registries.{Registry, RegistryError}

package object resolver {

  type Cache[F[_]] = CreateLruMap[F, SchemaKey, SchemaLookupStamped]

  /** Our LRU cache of schemas */
  type SchemaLruMap[F[_]] = LruMap[F, SchemaKey, SchemaLookupStamped]

  /**
   * Map of all repositories to its aggregated state of failure
   * None as value means repository already responded with `not-found`,
   * meaning all previous 500-like failures could probably been discarded
   */
  type RepoFailuresMap = Map[Registry, LookupHistory]

  /**
   * Validated schema lookup result containing, cache result which is
   * Json in case of Success or Map of all currently failed repositories
   * in case of Failure
   */
  type SchemaLookup = Either[RepoFailuresMap, Json]

  /** Schema lookup result associated with timestamp (in seconds) it was stored at */
  type SchemaLookupStamped = (Int, SchemaLookup)

  /** Our List (possibly empty) of Iglu repositories */
  type RepositoryRefs = List[Registry]

  type Result = Either[RegistryError, Json]

  type ResolveResult = Either[ResolverError, Json]

}
