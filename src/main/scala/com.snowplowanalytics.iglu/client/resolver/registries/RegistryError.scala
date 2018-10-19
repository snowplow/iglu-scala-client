/*
 * Copyright (c) 2018-2019 Snowplow Analytics Ltd. All rights reserved.
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
package com.snowplowanalytics.iglu.client.resolver.registries

sealed trait RegistryError

object RegistryError {

  /** Schema certainly does not exist on this registry */
  case object NotFound extends RegistryError

  /** Schema was there, but cannot be parsed */
  case class InvalidSchema(message: String) extends RegistryError

  /** Other error, e.g. 500 HTTP status */
  case class RepoFailure(message: String) extends RegistryError

  /** Internal error, usually due configuration error, can be considered fatal */
  case class ClientFailure(message: String) extends RegistryError
}
