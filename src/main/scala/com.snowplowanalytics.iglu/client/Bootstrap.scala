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

// This project
import com.snowplowanalytics.iglu.client.{Resolver => UrResolver}
import repositories.{
  RepositoryRefConfig,
  EmbeddedRepositoryRef
}

/**
 * Bootstrap object contains our embedded JSON
 * Schema repository, and a Resolver which
 * points exclusively to this embedded repo.
 */
object Bootstrap {

  // Keep this up-to-date
  private[client] val EmbeddedSchemaCount = 4

  // Our embedded JSON Schema repository.
  lazy val Repo = {
    val config = RepositoryRefConfig(
      name = "Iglu Client Embedded",
      instancePriority = 0,
      vendorPrefixes = Nil
    )
    EmbeddedRepositoryRef(config, path = "/iglu-client-embedded")    
  }

  // A Resolver which only looks at our embedded repo.
  lazy val Resolver = UrResolver(cacheSize = EmbeddedSchemaCount, repos = Nil, cacheTtl = None)
}
