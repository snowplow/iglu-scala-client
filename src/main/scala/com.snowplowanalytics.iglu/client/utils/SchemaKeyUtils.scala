/*
 * Copyright (c) 2018-2018 Snowplow Analytics Ltd. All rights reserved.
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
package utils

import cats.implicits._
import com.snowplowanalytics.iglu.core.SchemaKey

import validation.ProcessingMessage

private[client] object SchemaKeyUtils {

  def toPath(prefix: String, key: SchemaKey): String =
    s"$prefix/schemas/${key.vendor}/${key.name}/${key.format}/${key.version.asString}"

  def parseNel(uri: String): ValidatedNelType[SchemaKey] =
    SchemaKey
      .fromUri(uri)
      .toValidNel(ProcessingMessage(s"$uri is not a valid Iglu-format schema URI"))

}
