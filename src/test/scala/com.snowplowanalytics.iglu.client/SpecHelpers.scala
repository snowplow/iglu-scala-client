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

// Cats
import cats.effect.IO

// This project
import repositories.{EmbeddedRepositoryRef, HttpRepositoryRef, RepositoryRefConfig}
import com.snowplowanalytics.iglu.client.validation.ProcessingMessage

object SpecHelpers {

  val IgluCentral =
    HttpRepositoryRef(
      RepositoryRefConfig("Iglu Central", 0, List("com.snowplowanalytics")),
      "http://iglucentral.com")

  val EmbeddedTest =
    EmbeddedRepositoryRef(
      RepositoryRefConfig("Iglu Test Embedded", 0, List("com.snowplowanalytics")),
      path = "/iglu-test-embedded")

  val TestResolver = Resolver[IO](cacheSize = 10, EmbeddedTest)

  // TODO: improve this after ProcessingMessage format is discussed
  def asProcessingMessage(
    message: String,
    schema: String,
    instance: String,
    keyword: String,
    foundExpected: Option[(String, String)],
    requiredMissing: Option[(String, String)],
    unwanted: Option[String]): ProcessingMessage =
    ProcessingMessage(message = message)

}
