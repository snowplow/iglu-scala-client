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
package repositories

// Specs2
import org.specs2.Specification
import org.specs2.matcher.DataTables
import org.specs2.scalaz.ValidationMatchers

class EmbeddedRepositoryRefSpec extends Specification with DataTables with ValidationMatchers { def is =

  "This is a specification to test an embedded RepositoryRef"                                              ^
                                                                                                          p^
  // "retrieving an existent JSON Schema from an HTTP-based RepositoryRef should work"                     ! e1^
  "requesting a non-existent JSON Schema from an embedded RepositoryRef should return None"                ! e2^  
                                                                                                           end

  def e2 = {
    val schemaKey = SchemaKey("com.acme.n-a", "null", "jsonschema", "1-0-0")
    SpecHelpers.IgluCentral.lookupSchema(schemaKey) must beSuccessful(None)
  }

}
