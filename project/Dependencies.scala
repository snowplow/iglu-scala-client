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
import sbt._

object Dependencies {
  object V {
    // Java
    val commonsLang     = "3.1"
    val jacksonDatabind = "2.2.3"
    val jsonValidator   = "2.2.3"
    // Scala
    val circe           = "0.9.3"
    val json4s          = "3.2.11"
    val collUtils       = "6.20.0"
    // Scala (test only)
    val specs2          = "4.3.3"
    val mockito         = "1.10.19"
  }

  object Libraries {
    // Java
    val commonsLang      = "org.apache.commons"         %  "commons-lang3"           % V.commonsLang
    val jacksonDatabind  = "com.fasterxml.jackson.core" %  "jackson-databind"        % V.jacksonDatabind
    val jsonValidator    = "com.github.fge"             %  "json-schema-validator"   % V.jsonValidator
    // Scala
    val circeParser      = "io.circe"                   %% "circe-parser"            % V.circe
    val json4sJackson    = "org.json4s"                 %% "json4s-jackson"          % V.json4s
    val collUtils        = "com.twitter"                %% "util-collection"         % V.collUtils
    // Scala (test only)
    val specs2           = "org.specs2"                 %% "specs2-core"             % V.specs2          % "test"
    val specs2Cats       = "org.specs2"                 %% "specs2-cats"             % V.specs2          % "test"
    val specs2Mock       = "org.specs2"                 %% "specs2-mock"             % V.specs2          % "test"
    val mockito          = "org.mockito"                % "mockito-core"             % V.mockito         % "test"
  }
}
