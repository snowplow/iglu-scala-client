/*
 * Copyright (c) 2012-2023 Snowplow Analytics Ltd. All rights reserved.
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

import cats.effect.testing.specs2.CatsEffect
// circe
import io.circe.literal._

import com.snowplowanalytics.iglu.core.{SchemaKey, SchemaVer, SelfDescribingData}

// Specs2
import org.specs2.mutable.Specification

/** Similar to 'SchemaValidationSpec' and 'SelfDescValidationSpec' in validator package but using caching 'IgluCirceClient' client */
class CachingClientSpec extends Specification with CatsEffect {
  override def is = s2"""

  This is a specification to test client check

  validating a correct self-desc JSON should return the JSON in a Success $e1
  validating a correct self-desc JSON with JSON Schema with incorrect $$schema property should return Failure $e2
  validating an incorrect self-desc JSON should return the validation errors in a Failure  $e3
  validating a correct self-desc JSON with superseded schema should return the JSON in a Success $e4
  validating an incorrect self-desc JSON with superseded schema should return validation errors in a Failure  $e5
  validating self-desc JSONs with invalid superseded schemas should return resolution errors $e6

  """

  val validJson =
    SelfDescribingData(
      SchemaKey(
        "com.snowplowanalytics.iglu-test",
        "stock-item",
        "jsonschema",
        SchemaVer.Full(1, 0, 0)
      ),
      json"""{"id": "123-12", "name": "t-shirt", "price": 29.99 }"""
    )

  val invalidJson =
    SelfDescribingData(
      SchemaKey(
        "com.snowplowanalytics.iglu-test",
        "stock-item",
        "jsonschema",
        SchemaVer.Full(1, 0, 0)
      ),
      json"""{ "id": "123-12", "newName": "t-shirt", "price": "tbc"}"""
    )

  val validJsonWithInvalidSchema =
    SelfDescribingData(
      SchemaKey(
        "com.snowplowanalytics.iglu-test",
        "invalid-protocol",
        "jsonschema",
        SchemaVer.Full(1, 0, 0)
      ),
      json"""{"schema": "iglu://jsonschema/1-0-0", "data": { "id": 0 } }"""
    )

  val validJsonWithSupersededSchema1 =
    SelfDescribingData(
      SchemaKey(
        "com.snowplowanalytics.iglu-test",
        "superseded-schema",
        "jsonschema",
        SchemaVer.Full(1, 0, 0)
      ),
      json"""{ "id": "test-id" }"""
    )

  val validJsonWithSupersededSchema2 =
    SelfDescribingData(
      SchemaKey(
        "com.snowplowanalytics.iglu-test",
        "superseded-schema",
        "jsonschema",
        SchemaVer.Full(1, 0, 0)
      ),
      json"""{ "id": "test-id", "name": "test-name" }"""
    )

  val invalidJsonWithSupersededSchema =
    SelfDescribingData(
      SchemaKey(
        "com.snowplowanalytics.iglu-test",
        "superseded-schema",
        "jsonschema",
        SchemaVer.Full(1, 0, 0)
      ),
      json"""{ "id": "test-id", "name": "test-name", "field_a": "value_a" }"""
    )

  val jsonWithInvalidSupersededSchema100 =
    SelfDescribingData(
      SchemaKey(
        "com.snowplowanalytics.iglu-test",
        "invalid-superseded-schema",
        "jsonschema",
        SchemaVer.Full(1, 0, 0)
      ),
      json"""{ "id": "test-id" }"""
    )

  val jsonWithInvalidSupersededSchema101 =
    SelfDescribingData(
      SchemaKey(
        "com.snowplowanalytics.iglu-test",
        "invalid-superseded-schema",
        "jsonschema",
        SchemaVer.Full(1, 0, 1)
      ),
      json"""{ "id": "test-id" }"""
    )

  val jsonWithInvalidSupersededSchema102 =
    SelfDescribingData(
      SchemaKey(
        "com.snowplowanalytics.iglu-test",
        "invalid-superseded-schema",
        "jsonschema",
        SchemaVer.Full(1, 0, 2)
      ),
      json"""{ "id": "test-id" }"""
    )

  def e1 = {
    for {
      client <- SpecHelpers.CachingTestClient
      result <- client.check(validJson).value
    } yield result must beRight
  }

  def e2 = {
    for {
      client <- SpecHelpers.CachingTestClient
      result <- client.check(validJsonWithInvalidSchema).value
    } yield result must beLeft
  }

  def e3 = {
    for {
      client <- SpecHelpers.CachingTestClient
      result <- client.check(invalidJson).value
    } yield result must beLeft
  }

  def e4 = {
    val supersedingSchema = SchemaVer.Full(1, 0, 2)
    for {
      res1 <- for {
        client <- SpecHelpers.CachingTestClient
        result <- client.check(validJsonWithSupersededSchema1).value
      } yield result
      res2 <- for {
        client <- SpecHelpers.CachingTestClient
        result <- client.check(validJsonWithSupersededSchema2).value
      } yield result
    } yield (res1 must beRight(Some(supersedingSchema))) and
      (res2 must beRight(Some(supersedingSchema)))
  }

  def e5 = {
    for {
      client <- SpecHelpers.CachingTestClient
      result <- client.check(invalidJsonWithSupersededSchema).value
    } yield {
      result must beLeft.like {
        case (_, Some(v: SchemaVer.Full)) if v == SchemaVer.Full(1, 0, 2) => ok
        case _                                                            => ko
      }
    }
  }

  def e6 = {
    for {
      res1 <- for {
        client <- SpecHelpers.CachingTestClient
        result <- client.check(jsonWithInvalidSupersededSchema100).value
      } yield result
      res2 <- for {
        client <- SpecHelpers.CachingTestClient
        result <- client.check(jsonWithInvalidSupersededSchema101).value
      } yield result
      res3 <- for {
        client <- SpecHelpers.CachingTestClient
        result <- client.check(jsonWithInvalidSupersededSchema102).value
      } yield result
    } yield {
      val match1 = res1.toString must contain("Invalid schema version: 1-0")
      val match2 = res2.toString must contain("Iglu Test Embedded -> LookupHistory(Set(NotFound)")
      val match3 = res3.toString must contain(
        "ClientFailure(Superseding version 1-0-1 isn't greater than the version of schema com.snowplowanalytics.iglu-test/invalid-superseded-schema/jsonschema/1-0-2)"
      )
      match1.and(match2).and(match3)
    }
  }

}
