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
package com.snowplowanalytics.iglu.client.resolver

import cats.effect.IO
import cats.effect.testing.specs2.CatsEffect

import io.circe.literal._
import io.circe.Json
import io.circe.syntax._

import com.snowplowanalytics.iglu.core.{SchemaKey, SchemaMap, SchemaVer, SelfDescribingSchema}
import com.snowplowanalytics.iglu.client.resolver.registries.Registry
import com.snowplowanalytics.iglu.client.resolver.registries.JavaNetRegistryLookup._
import com.snowplowanalytics.iglu.client.SpecHelpers

import org.specs2.Specification

class ResolverConfigSpec extends Specification with CatsEffect {
  def is = s2"""
  Resolver configuration should
    validateRefs should succeed for unique names     $e1
    validateRefs should fail for duplicate names     $e2
    initPure should create resolver without cache    $e3
    initPure should work with InMemory registries    $e4
    parseConfig should fail for wrong schema         $e5
    bootstrap should create a resolver               $e6
    shouldCreateResolverCache returns false for 0 size $e7
    shouldCreateResolverCache returns true for positive size $e8
  """

  def e1 = {
    val repos = List(
      Registry.Embedded(Registry.Config("repo-a", 0, Nil), "/a"),
      Registry.Embedded(Registry.Config("repo-b", 0, Nil), "/b")
    )
    Resolver.validateRefs(repos) must beRight(())
  }

  def e2 = {
    val repos = List(
      Registry.Embedded(Registry.Config("same-name", 0, Nil), "/a"),
      Registry.Embedded(Registry.Config("same-name", 0, Nil), "/b")
    )
    Resolver.validateRefs(repos) must beLeft
  }

  def e3 = {
    val inMemory = Registry.InMemory(Registry.Config("test", 0, Nil), Nil)
    val resolver = Resolver.initPure(inMemory)
    (resolver.repos must contain(inMemory)) and
      (resolver.cache must beNone)
  }

  def e4 = {
    import SpecHelpers.idClock
    val key      = SchemaKey("com.test", "test-schema", "jsonschema", SchemaVer.Full(1, 0, 0))
    val schema   = SelfDescribingSchema(SchemaMap(key), Json.obj("type" := "object"))
    val inMemory = Registry.InMemory(Registry.Config("test", 0, Nil), List(schema))
    val resolver = Resolver.initPure(inMemory)

    val result = resolver.lookupSchema(key)
    result must beRight
  }

  def e5 = {
    val badConfig = json"""{
      "schema": "iglu:com.example/wrong-schema/jsonschema/1-0-0",
      "data": {
        "cacheSize": 10,
        "repositories": []
      }
    }"""
    Resolver.parseConfig(badConfig) must beLeft
  }

  def e6 = {
    Resolver.bootstrap[IO].map { resolver =>
      resolver.repos must not(beEmpty)
    }
  }

  def e7 =
    ResolverCache.shouldCreateResolverCache(0, None) must beFalse

  def e8 =
    ResolverCache.shouldCreateResolverCache(10, None) must beTrue
}
