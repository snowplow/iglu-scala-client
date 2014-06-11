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

// JSON Schema
import com.github.fge.jsonschema.core.report.{
  ProcessingMessage,
  LogLevel
}

object ValidationSpecHelpers {

  private lazy val Mapper = new ObjectMapper
  
  def asJsonNode(str: String) =
    Mapper.readTree(str)

  def asProcessingMessage(message: String, schema: String, instance: String, keyword: String, foundExpected: Option[(String, String)], requiredMissing: Option[(String, String)]) = {

    val pm = new ProcessingMessage()
                   .setLogLevel(LogLevel.ERROR)
                   .setMessage(message)
                   .put("schema",   asJsonNode(schema))
                   .put("instance", asJsonNode(instance))
                   .put("domain",  "validation")
                   .put("keyword",  keyword)

    foundExpected match {
      case Some(Tuple2(found, expected)) =>
        pm.put("found",    found)
        pm.put("expected", asJsonNode(expected))
      case _ =>
    }
    requiredMissing match {
      case Some(Tuple2(required, missing)) =>
        pm.put("required", asJsonNode(required))
        pm.put("missing",  asJsonNode(missing))
      case _ =>
    }

    pm
  }

}
