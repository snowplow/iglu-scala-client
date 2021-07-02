/*
 * Copyright (c) 2014-2020 Snowplow Analytics Ltd. All rights reserved.
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
package validator

import io.circe._
import io.circe.syntax._

case class ValidatorReport(
  message: String,
  path: Option[String],
  targets: List[String],
  keyword: Option[String]
)

object ValidatorReport {
  implicit val validatorReportCirceEncoder: Encoder[ValidatorReport] =
    Encoder.instance { report =>
      Json.obj(
        "message" := report.message,
        "path" := report.path,
        "keyword" := report.keyword,
        "targets" := report.targets
      )
    }

  implicit val validatorReportCirceDecoder: Decoder[ValidatorReport] =
    Decoder.instance { cursor =>
      for {
        message <- cursor.downField("message").as[String]
        path    <- cursor.downField("path").as[Option[String]]
        targets <- cursor.downField("targets").as[Option[List[String]]].map(_.getOrElse(List.empty))
        keyword <- cursor.downField("keyword").as[Option[String]]
      } yield ValidatorReport(message, path, targets, keyword)
    }
}
