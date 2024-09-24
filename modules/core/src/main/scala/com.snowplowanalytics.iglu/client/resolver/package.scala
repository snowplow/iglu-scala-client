/*
 * Copyright (c) 2014-2023 Snowplow Analytics Ltd. All rights reserved.
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

import scala.concurrent.duration.FiniteDuration

import cats.data.NonEmptyList

import io.circe.Json

// Iglu Core
import com.snowplowanalytics.iglu.core.{SchemaList, SelfDescribingSchema}

// This project
import resolver.registries.Registry
import resolver.Resolver.SchemaItem

package object resolver {

  /** Schema's vendor */
  type Vendor = String

  /** Schema's name */
  type Name = String

  /** Schema's model */
  type Model = Int

  type SchemaContentList = NonEmptyList[SelfDescribingSchema[Json]]

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
  type SchemaLookup = Either[LookupFailureMap, SchemaItem]

  /**
   * Validated schema list lookup result containing, cache result which is
   * Json in case of success or Map of all currently failed repositories
   * in case of failure
   */
  type ListLookup = Either[LookupFailureMap, SchemaList]

  /**
   * Validated schema content list lookup result containing, cache result
   * which is list of self describing schemas in case of success or
   * Map of all currently failed repositories in case of failure
   */
  type SchemaContentListLookup = Either[LookupFailureMap, SchemaContentList]

  /** Time to live for cached items */
  type TTL = FiniteDuration

  /** Indicates the moment in time when item has been stored in the cache */
  type StorageTime = FiniteDuration

  /**
   * Key to identify stored schema list in the cache.
   * Consists of the schema's vendor, name and model
   */
  type ListCacheKey = (Vendor, Name, Model)

  /**
   * Single entry stored in the cache.
   * Entry consists of the time item has been stored and the item itself.
   */
  type CacheEntry[A] = (StorageTime, A)

  /** Cache entry for schema lookup results */
  type SchemaCacheEntry = CacheEntry[SchemaLookup]

  /** Cache entry for schema list lookup results */
  type ListCacheEntry = CacheEntry[ListLookup]

  /** Cache entry for schema content list lookup results */
  type SchemaContentListCacheEntry = CacheEntry[SchemaContentListLookup]

}
