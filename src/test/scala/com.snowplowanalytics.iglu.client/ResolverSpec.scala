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

// Java
import java.net.URL

// Scalaz
import scalaz._
import Scalaz._

// This project
import repositories.{
  EmbeddedRepositoryRef,
  HttpRepositoryRef,
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
    val two   = embedRef("de.acompany.snowplow", 40)
    val three = embedRef("de.acompany.snowplow", 100)

    // TODO: update this once we have our HttpRepositoryRef
    val igluCentral =
      HttpRepositoryRef(RepositoryRefConfig("Iglu Central", 0, List("com.snowplowanalytics")), new URL("http://iglucentral.com"))
  }
}

class ResolverSpec extends Specification with DataTables with ValidationMatchers { def is =

  "This is a specification to test the Resolver functionality"                   ^
                                                                                p^
  "our prioritizeRepos algorithm should sort repository refs in priority order"  ! e1^
  "we can construct a Resolver from a valid resolver configuration JSON"         ! e2^
                                                                                 end

  import ResolverSpec._

  def e1 = {
    val resolver = Resolver(cacheSize = 0, Repos.igluCentral, Repos.one, Repos.two, Repos.three)
    val schemaKey = SchemaKey("de.acompany.snowplow", "mobile_context", "jsonschema", "1-0-0")

    resolver.prioritizeRepos(schemaKey) must_== List(Repos.two, Repos.three, Bootstrap.Repo, Repos.one, Repos.igluCentral)
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

    Resolver.parse(SpecHelpers.asJsonNode(config)) must beSuccessful(expected)
  }

}
