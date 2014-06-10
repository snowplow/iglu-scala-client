/*
 * Copyright (c) 2012-2014 Snowplow Analytics Ltd. All rights reserved.
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

// Scalaz
import scalaz._
import Scalaz._

// This project
import repositories.{
  EmbeddedRepositoryRef,
  RepositoryRefConfig
}

// Specs2
import org.specs2.Specification
import org.specs2.matcher.DataTables
import org.specs2.scalaz.ValidationMatchers

class ResolverSpec extends Specification with DataTables with ValidationMatchers { def is =

  "This is a specification to test the Resolver functionality"                   ^
                                                                                p^
  "our prioritizeRepos algorithm should sort repository refs in priority order"  ! e1^
                                                                                 end

  // TODO: update this test once we have an HttpRepositoryRef to test against too
  def e1 = {

    object Repos { 
      private val getRef: (String, Int) => EmbeddedRepositoryRef = (prefix, priority) =>
        EmbeddedRepositoryRef(RepositoryRefConfig("n/a", priority, List(prefix)), "n/a")

      val one   = getRef("com.zendesk", 0)
      val two   = getRef("com.snowplowanalytics.snowplow", 40)
      val three = getRef("com.snowplowanalytics.snowplow", 100)
    }

    val resolver = Resolver(NonEmptyList(Repos.one, Repos.two, Repos.three), lruCache = 0)
    val schemaKey = SchemaKey("com.snowplowanalytics.snowplow", "mobile_context", "jsonschema", "1-0-0")

    resolver.prioritizeRepos(schemaKey) must_== List(Repos.two, Repos.three, Repos.one)
  }

}
