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
package utils

// TODO
object ValidationExceptions {
  
  /**
   * Strips the instance information from a Jackson
   * parsing exception message:
   *
   * "... at [Source: java.io.StringReader@1fe7a8f8; line: 1, column: 2]""
   *                                       ^^^^^^^^
   *
   * Because this makes it hard to test.
   *
   * @param message The exception message which is
   *        leaking instance information
   * @return the same exception message, but with
   *         instance information removed
   */
  def stripInstanceEtc(message: String): String = {
    message
    .replaceAll("@[0-9a-z]+;", "@xxxxxx;")
    .replaceAll("\\t", "    ")
    .replaceAll("\\p{Cntrl}", "") // Any other control character
    .trim
  }
}
