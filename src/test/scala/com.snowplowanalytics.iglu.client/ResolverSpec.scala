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

object ResolverSpec {

  object Repos { 

    private val embedRef: (String, Int) => EmbeddedRepositoryRef = (prefix, priority) =>
      EmbeddedRepositoryRef(RepositoryRefConfig("An embedded repo", priority, List(prefix)), "/embed-path")

    val one   = embedRef("com.acme", 0)
    val two   = embedRef("com.snowplowanalytics.snowplow", 40)
    val three = embedRef("com.snowplowanalytics.snowplow", 100)

    // TODO: update this once we have our HttpRepositoryRef
    val igluCentral = embedRef("com.snowplowanalytics", 0)
  }
}

class ResolverSpec extends Specification with DataTables with ValidationMatchers { def is =

  "This is a specification to test the Resolver functionality"                   ^
                                                                                p^
  "our prioritizeRepos algorithm should sort repository refs in priority order"  ! e1^
  "we should be able to construct a Resolver from a resolver configuration JSON" ! e2^
                                                                                 end

  import ResolverSpec._

  // TODO: update this test once we have an HttpRepositoryRef to test against too
  def e1 = {
    val resolver = Resolver(cacheSize = 0, Repos.one, Repos.two, Repos.three)
    val schemaKey = SchemaKey("com.snowplowanalytics.snowplow", "mobile_context", "jsonschema", "1-0-0")

    resolver.prioritizeRepos(schemaKey) must_== List(Repos.two, Repos.three, Bootstrap.Repo, Repos.one)
  }

  def e2 = {

    val config = 
      s"""|{
            |"schema": "iglu:com.snowplowanalytics.iglu/resolver-config/jsonschema/1-0-0",
            |"data": {
              |"cacheSize": 500,
              |"repositories": [
                |{
                  |"name": "Iglu Central",
                  |"priority": 0,
                  |"vendorPrefixes": [ "com.snowplowanalytics" ],
                  |"connection": {
                    |"http": {
                      |"uri": "http://iglucentral.com"
                    |}
                  |}
                |}, {                
                  |"name": "An embedded repo",
                  |"priority": 100,
                  |"vendorPrefixes": [ "com.snowplowanalytics.snowplow" ],
                  |"connection": {
                    |"embedded": {
                      |"path": "/embed-path"
                    |}
                  |}
                |}
              |]
            |}
          |}""".stripMargin.replaceAll("[\n\r]","")

    val expected = Resolver(cacheSize = 500, Repos.igluCentral, Repos.three)

    Resolver(SpecHelpers.asJsonNode(config)) must beSuccessful(expected)
  }

}
