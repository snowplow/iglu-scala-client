/*
 * Copyright (c) 2014-2019 Snowplow Analytics Ltd. All rights reserved.
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
import sbt._

object Dependencies {
  object V {
    // Java
    val jsonValidator   = "1.0.20"
    val slf4j           = "1.7.26"
    // Scala
    val igluCore        = "0.5.1"
    val cats            = "1.6.1"
    val circe           = "0.11.1"
    val lruMap          = "0.3.0"
    val scalaj          = "2.4.2"
    // Scala (test only)
    val specs2          = "4.3.3"
  }

  object Libraries {
    // Java
    val jsonValidator    = "com.networknt"              %  "json-schema-validator"   % V.jsonValidator
    // Scala
    val igluCore         = "com.snowplowanalytics"      %% "iglu-core"               % V.igluCore
    val igluCoreCirce    = "com.snowplowanalytics"      %% "iglu-core-circe"         % V.igluCore
    val cats             = "org.typelevel"              %% "cats-core"               % V.cats
    val circeParser      = "io.circe"                   %% "circe-parser"            % V.circe
    val circeTime        = "io.circe"                   %% "circe-java8"             % V.circe
    val circeJackson     = "io.circe"                   %% "circe-jackson29"         % V.circe
    val lruMap           = "com.snowplowanalytics"      %% "scala-lru-map"           % V.lruMap
    val scalaj           = "org.scalaj"                 %% "scalaj-http"             % V.scalaj
    // Scala (test only)
    val circeLiteral     = "io.circe"                   %% "circe-literal"           % V.circe           % Test
    val specs2           = "org.specs2"                 %% "specs2-core"             % V.specs2          % Test
    val specs2Cats       = "org.specs2"                 %% "specs2-cats"             % V.specs2          % Test
    // Java (exists to suppress NOP log message, must not be included in compile-time)
    val slf4jNop         = "org.slf4j"                  % "slf4j-nop"                % V.slf4j           % Test
  }
}
