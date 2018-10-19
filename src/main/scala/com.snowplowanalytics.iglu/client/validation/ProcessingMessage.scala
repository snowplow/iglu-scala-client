/*
 * Copyright (c) 2014-2018 Snowplow Analytics Ltd. All rights reserved.
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

// Circe
import io.circe._
import io.circe.syntax._

// TODO: replace with better format after discussion
case class ProcessingMessage(
  message: String,
  jsonPath: Option[String] = None,
  targets: Option[List[String]] = None,
  schemaKey: Option[String] = None,
  keyword: Option[String] = None,
  repositories: Option[List[String]] = None) {

  def asJson: Json = {
    Json.obj(
      "message" := message,
      "path" := jsonPath,
      "schemaKey" := schemaKey,
      "keyword" := keyword,
      "targets" := targets,
      "repositories" := repositories
    )
  }
}
