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
package validation

// Scalaz
import scalaz._
import Scalaz._

// Specs2
import org.specs2.Specification
import org.specs2.matcher.DataTables
import org.specs2.scalaz.ValidationMatchers

class SchemaCriterionSpec extends Specification with DataTables with ValidationMatchers { def is = s2"""

  This is a specification to test the SchemaCriterion class

  if the payload, vendor, and format aren't identical, the schema should be rejected  $e1
  correctly validate schemas  $e2
  parse schema criterion  $e3
  """

  def e1 = {
    val actual = SchemaKey("wrong", "payload_data", "jsonschema", "2-3-4")
    SchemaCriterion("com.snowplowanalytics.snowplow", "payload_data", "jsonschema", 1).matches(actual) must beFalse
  }

  def e2 =
    "SPEC NAME"                                         || "Criterion"           | "SchemaVer" | "EXPECTED OUTPUT" |
    "Correct model"                                     !! (2, None,    None)    ! "2-3-4"     ! true              |
    "Incorrect model version"                           !! (2, None,    None)    ! "1-0-0"     ! false             |
    "Correct revision"                                  !! (2, Some(3), None)    ! "2-3-0"     ! true              |
    "Correct revision and addition"                     !! (2, Some(3), None)    ! "2-0-9"     ! true              |
    "Incorrect revision"                                !! (2, Some(3), None)    ! "2-4-0"     ! false             |
    "Correct model, revision, and addition"             !! (2, Some(3), Some(4)) ! "2-3-4"     ! true              |
    "Correct model and revision, higher addition"       !! (2, Some(3), Some(4)) ! "2-3-9"     ! false             |
    "Correct model, lower revision, higher addition"    !! (2, Some(3), Some(4)) ! "2-0-9"     ! true              |>{
      (_, criterion, version, expected) =>
        SchemaCriterion("com.snowplowanalytics.snowplow", "payload_data", "jsonschema", criterion._1, criterion._2, criterion._3)
          .matches(SchemaKey("com.snowplowanalytics.snowplow", "payload_data", "jsonschema", version))
          .must_==(expected)
    }

  def e3 = {
    val criterion = SchemaCriterion.parse("iglu:com.snowplowanalytics.snowplow/mobile_context/jsonschema/1-0-*")
    criterion must beSuccessful(SchemaCriterion("com.snowplowanalytics.snowplow", "mobile_context", "jsonschema", 1, Some(0), None))
  }

}
