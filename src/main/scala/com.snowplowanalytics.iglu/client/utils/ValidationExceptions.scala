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

// Apache Commons
import org.apache.commons.lang3.exception.ExceptionUtils

/**
 * Provides helpers around converting JVM
 * exceptions to Scalaz Validations.
 */
object ValidationExceptions {
  
  /**
   * Strips the instance information from a Jackson
   * parsing exception message:
   *
   * "... at [Source: java.io.StringReader@1fe7a8f8; line: 1, column: 2]""
   *                                       ^^^^^^^^
   *
   * Also removes any control characters and replaces
   * tabs with 4 spaces.
   *
   * @param message The exception message which needs
   *        tidying up
   * @return the same exception message, but with
   *         instance information etc removed
   */
  def stripInstanceEtc(message: String): String = {
    message
    .replaceAll("@[0-9a-z]+;", "@xxxxxx;")
    .replaceAll("\\t", "    ")
    .replaceAll("\\p{Cntrl}", "") // Any other control character
    .trim
  }

  /**
   * Get the message out of a Throwable's root cause
   * (or failing that the Throwable itself) in a
   * null-safe fashion.
   *
   * @param throwable The throwable to extract a message from
   * @return the message from either the Throwable or
   *         preferably its root cause, Option-boxed
   */
  def getThrowableMessage(throwable: Throwable): Option[String] = {

    def getRootCauseIfExists(throwable: Throwable): Throwable =
      Option(ExceptionUtils.getRootCause(throwable)) match {
        case Some(root) => root
        case None       => throwable
      }

    for {
      t  <- Option(throwable)
      rc =  getRootCauseIfExists(t)
      m  <- Option(rc.getMessage)
    } yield m
  }

}
