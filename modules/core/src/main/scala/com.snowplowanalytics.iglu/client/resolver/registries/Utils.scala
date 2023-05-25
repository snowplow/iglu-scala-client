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
package com.snowplowanalytics.iglu.client
package resolver.registries

// Java
import java.net.URI

// Cats
import cats.syntax.either._
import cats.syntax.show._

// circe
import io.circe.ParsingFailure

// Apache Commons
import org.apache.commons.lang3.exception.ExceptionUtils

private[registries] object Utils {

  /**
   * A wrapper around Java's URI.
   *
   * @param url The String to convert to a URI
   * @return an URI, or an error message, all wrapped in an Either
   */
  def stringToUri(url: String): Either[RegistryError, URI] =
    try URI.create(url).asRight
    catch {
      case _: NullPointerException =>
        RegistryError.ClientFailure("Provided URL was null").asLeft
      case e: IllegalArgumentException =>
        val error = ExceptionUtils.getRootCause(e).getMessage
        RegistryError.ClientFailure(s"Provided URI string violates RFC 2396: [$error]").asLeft
    }

  def invalidSchema(failure: ParsingFailure): RegistryError =
    RegistryError.RepoFailure(failure.show)

  def repoFailure(failure: Throwable): RegistryError =
    RegistryError.RepoFailure(
      if (failure.getMessage != null) failure.getMessage else "Unhandled error"
    )

}
