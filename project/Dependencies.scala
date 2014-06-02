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
  val resolutionRepos = Seq(
    "Twitter Maven Repo" at "http://maven.twttr.com/" // For Twitter's util functions
  )

  object V {
    // Java
    val jacksonDatabind = "2.2.3"
    val jsonValidator   = "2.2.3"
    // Scala
    val json4sJackson   = "3.2.11-SNAPSHOT"
    val scalaz7         = "7.0.0"
    val collUtils       = "6.3.4"
    // Scala (test only)
    val specs2          = "1.14" // Downgrade to prevent issues in job tests. WAS: "2.3.11"
    val scalazSpecs2    = "0.1.2"
  }

  object Libraries {
    // Java
    val jacksonDatabind  = "com.fasterxml.jackson.core" %  "jackson-databind"        % V.jacksonDatabind
    val jsonValidator    = "com.github.fge"             %  "json-schema-validator"   % V.jsonValidator
    // Scala
    val json4sJackson    = "org.json4s"                 %% "json4s-jackson"          % V.json4sJackson
    val scalaz7          = "org.scalaz"                 %% "scalaz-core"             % V.scalaz7
    val collUtils        = "com.twitter"                %% "util-collection"         % V.collUtils
    // Scala (test only)
    val specs2           = "org.specs2"                 %% "specs2"                  % V.specs2          % "test"
    val scalazSpecs2     = "org.typelevel"              %% "scalaz-specs2"           % V.scalazSpecs2    % "test"
  }
}
