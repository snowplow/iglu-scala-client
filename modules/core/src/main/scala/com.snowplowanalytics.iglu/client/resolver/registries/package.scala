/*
 * Copyright (c) 2014-2021 Snowplow Analytics Ltd. All rights reserved.
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
package com.snowplowanalytics.iglu.client.resolver

import scala.util.{Either, Right}

package object registries {

  /** Workaround for 2.12/2.13 compatibility */
  implicit class EitherOps[A, B](either: Either[A, B]) {
    def orElse[A1 >: A, B1 >: B](or: => Either[A1, B1]): Either[A1, B1] =
      either match {
        case Right(_) => either
        case _        => or
      }
  }
}
