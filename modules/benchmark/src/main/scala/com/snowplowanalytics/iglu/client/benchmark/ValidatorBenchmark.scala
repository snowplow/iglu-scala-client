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
package com.snowplowanalytics.iglu.client.benchmark

import java.util.concurrent.TimeUnit

import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.Blackhole

import io.circe.Json
import io.circe.parser.{parse => parseJson}

import com.snowplowanalytics.iglu.client.validator.CirceValidator

@State(Scope.Benchmark)
@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.SECONDS)
@Warmup(iterations = 5, time = 1)
@Measurement(iterations = 10, time = 1)
@Fork(1)
class ValidatorBenchmark {

  var schema: Json = _
  var instances: Array[Json] = _
  var compiled: CirceValidator.CompiledSchema = _

  @Setup(Level.Trial)
  def setup(): Unit = {
    schema = {
      val stream = getClass.getResourceAsStream("/draft-04/schema.json")
      val raw = scala.io.Source.fromInputStream(stream, "UTF-8").mkString
      stream.close()
      parseJson(raw).fold(throw _, identity)
    }

    instances = {
      val stream = getClass.getResourceAsStream("/draft-04/instances.jsonl")
      val lines = scala.io.Source.fromInputStream(stream, "UTF-8").getLines().toArray
      stream.close()
      lines.map(line => parseJson(line).fold(throw _, identity))
    }

    compiled = CirceValidator.compileJsonSchema(schema, Int.MaxValue).fold(
      e => throw new RuntimeException(s"Schema compilation failed: $e"),
      identity
    )
  }

  @Benchmark
  def compileAndValidate(bh: Blackhole): Unit = {
    var i = 0
    while (i < instances.length) {
      bh.consume(CirceValidator.validate(instances(i), schema))
      i += 1
    }
  }

  @Benchmark
  def validate(bh: Blackhole): Unit = {
    var i = 0
    while (i < instances.length) {
      bh.consume(CirceValidator.validateCompiled(instances(i), compiled))
      i += 1
    }
  }

  @Benchmark
  def checkSchema(bh: Blackhole): Unit = {
    bh.consume(CirceValidator.checkSchema(schema, Int.MaxValue))
  }
}
