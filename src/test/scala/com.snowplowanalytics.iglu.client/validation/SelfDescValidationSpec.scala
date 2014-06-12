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
package validation

// Jackson
import com.fasterxml.jackson.databind.{
  ObjectMapper,
  JsonNode
}
import com.github.fge.jackson.JsonLoader
import com.github.fge.jsonschema.core.report.{
  ProcessingMessage,
  LogLevel
}

// This project
import repositories.{
  RepositoryRefConfig,
  EmbeddedRepositoryRef
}

// Specs2
import org.specs2.Specification
import org.specs2.matcher.DataTables
import org.specs2.scalaz.ValidationMatchers

object SelfDescValidationSpec {

  implicit val resolver = {
    val repo = {
      val config = RepositoryRefConfig(
        name = "Iglu Test Embedded",
        instancePriority = 0,
        vendorPrefixes = Nil
      )
      EmbeddedRepositoryRef(config, path = "/iglu-test-embedded")    
    }

    // Disable LRU cache as not thread-safe for tests
    Resolver(cacheSize = 0, repo)
  }
}

// TODO: add in some Failures
class SelfDescValidationSpec extends Specification with DataTables with ValidationMatchers { def is =

  "This is a specification to test validation of self-describing JsonNodes"                                ^
                                                                                                          p^
  "validating a correct self-desc JSON should return the JSON in a Success"                                ! e1^
  "validating a correct self-desc JSON should return only the JSON's data field in a Success if requested" ! e2^
  // "JsonNodes that fail explicit validation should wrap ProcessageMessages in a Failure" ! e2^  
                                                                                           end

  import ValidatableJsonMethods._
  import SelfDescValidationSpec._

  val validJson = SpecHelpers.asJsonNode(
    """{"schema": "iglu:com.snowplowanalytics.iglu-test/stock-item/jsonschema/1-0-0", "data": { "id": "123-12", "name": "t-shirt", "price": 29.99 } }"""
    )

  def e1 = validJson.validate(false) must beSuccessful(validJson)

  def e2 = validJson.validate(true) must beSuccessful(validJson.get("data"))

}
