/*
 * Copyright (c) 2014 Snowplow Analytics Ltd. All rights reserved.
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

// Jackson
import com.fasterxml.jackson.databind.JsonNode

// LRU
import com.twitter.util.LruMap

// Scalaz
import scalaz._
import Scalaz._

// This project
import repositories.RepositoryRef

/**
 * Resolves schemas from one or more Iglu schema
 * repositories.
 *
 * This is an extremely primitive implementation.
 * Currently it only supports access to locally
 * stored schemas specified by the exact same
 * version (i.e. MODEL-REVISION-ADDITION).
 */
class Resolver(repos: NonEmptyList[RepositoryRef], mode: ResolutionMode, lruCache: Int = 500) {
  
  // Initialise the cache
  private val lru = if (lruCache > 0) Some(new LruMap[SchemaKey, JsonNode](lruCache)) else None

}
