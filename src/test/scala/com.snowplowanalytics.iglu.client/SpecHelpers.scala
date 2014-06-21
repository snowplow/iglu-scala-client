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

// Java
import java.net.URL

// JSON Schema
import com.github.fge.jsonschema.core.report.{
  ProcessingMessage,
  LogLevel
}

// json4s
import org.json4s._
import org.json4s.JsonDSL._
import org.json4s.jackson.JsonMethods.{
  parse,
  asJsonNode => ajn
}

// This project
import repositories.{
  EmbeddedRepositoryRef,
  HttpRepositoryRef,
  RepositoryRefConfig
}

object SpecHelpers {
  
  val IgluCentral =
    HttpRepositoryRef(RepositoryRefConfig("Iglu Central", 0, List("com.snowplowanalytics")), new URL("http://iglucentral.com"))

  val EmbeddedTest =
    EmbeddedRepositoryRef(RepositoryRefConfig("Iglu Test Embedded", 0, List("com.snowplowanalytics")), path = "/iglu-test-embedded") 

  val TestResolver = Resolver(cacheSize = 10, EmbeddedTest)

  def asJValue(str: String) =
    parse(str)

  def asJsonNode(str: String) =
    ajn(asJValue(str))

  def asProcessingMessage(message: String, schema: String, instance: String, keyword: String, foundExpected: Option[(String, String)], requiredMissing: Option[(String, String)], unwanted: Option[String]) = {

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
    for (unw <- unwanted) {
      pm.put("unwanted", asJsonNode(unw))
    }

    pm
  }

}
