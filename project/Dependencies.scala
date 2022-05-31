/* Copyright (c) 2014-2022 Snowplow Analytics Ltd. All rights reserved.
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
    val validator = "1.0.70"
    val slf4j     = "1.7.30"
    val jackson   = "2.13.3"

    // Scala
    val igluCore         = "1.1.0"
    val cats             = "2.7.0"
    val catsEffect       = "3.3.12"
    val circe            = "0.14.2"
    val lruMap           = "0.6.0"
    val collectionCompat = "2.7.0"
    val http4s           = "0.23.12"

    // Scala (test only)
    val specs2           = "4.15.0"
    val specs2CatsEffect = "1.4.0"
  }

  object Libraries {
    // Java
    val validator = "com.networknt"              % "json-schema-validator" % V.validator
    val jackson   = "com.fasterxml.jackson.core" % "jackson-databind"      % V.jackson

    // Scala
    val igluCore         = "com.snowplowanalytics"  %% "iglu-core"               % V.igluCore
    val igluCoreCirce    = "com.snowplowanalytics"  %% "iglu-core-circe"         % V.igluCore
    val cats             = "org.typelevel"          %% "cats-core"               % V.cats
    val catsEffect       = "org.typelevel"          %% "cats-effect"             % V.catsEffect
    val circeCore        = "io.circe"               %% "circe-core"              % V.circe       
    val circeParser      = "io.circe"               %% "circe-parser"            % V.circe
    val lruMap           = "com.snowplowanalytics"  %% "scala-lru-map"           % V.lruMap
    val collectionCompat = "org.scala-lang.modules" %% "scala-collection-compat" % V.collectionCompat
    val http4sClient     = "org.http4s"             %% "http4s-client"           % V.http4s
    val http4sCirce      = "org.http4s"             %% "http4s-circe"            % V.http4s

    // Scala (test only)
    val circeLiteral     = "io.circe"      %% "circe-literal"              % V.circe            % Test
    val circeJawn        = "io.circe"      %% "circe-jawn"                 % V.circe            % Test
    val specs2           = "org.specs2"    %% "specs2-core"                % V.specs2           % Test
    val specs2Cats       = "org.specs2"    %% "specs2-cats"                % V.specs2           % Test
    val specs2CatsEffect = "org.typelevel" %% "cats-effect-testing-specs2" % V.specs2CatsEffect % Test
    val http4sDsl        = "org.http4s"    %% "http4s-dsl"                 % V.http4s           % Test

    // Java (exists to suppress NOP log message, must not be included in compile-time)
    val slf4jNop = "org.slf4j" % "slf4j-nop" % V.slf4j % Test
  }
}
