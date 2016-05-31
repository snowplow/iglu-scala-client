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
    val commonsLang     = "3.1"
    val jacksonDatabind = "2.2.3"
    val jsonValidator   = "2.2.3"
    // Scala
    val json4s          = "3.2.11"
    val scalaz7         = "7.0.0"
    val collUtils       = "6.20.0"
    // Scala (test only)
    val specs2          = "2.3.13" // Downgrade to prevent issues in job tests. WAS: "2.3.11"
    val scalazSpecs2    = "0.2"
    val mockito         = "1.10.19"
    val hadoop          = "2.2.0"
  }

  object Libraries {
    // Java
    val commonsLang      = "org.apache.commons"         %  "commons-lang3"           % V.commonsLang
    val jacksonDatabind  = "com.fasterxml.jackson.core" %  "jackson-databind"        % V.jacksonDatabind
    val jsonValidator    = "com.github.fge"             %  "json-schema-validator"   % V.jsonValidator
    // Scala
    val json4sJackson    = "org.json4s"                 %% "json4s-jackson"          % V.json4s
    val json4sScalaz     = "org.json4s"                 %% "json4s-scalaz"           % V.json4s
    val scalaz7          = "org.scalaz"                 %% "scalaz-core"             % V.scalaz7
    val collUtils        = "com.twitter"                %% "util-collection"         % V.collUtils
    // hadoop hdfs
    val hadoopCommon     = "org.apache.hadoop"         %  "hadoop-common"            % V.hadoop		 % "provided"
    val hadoopHdfs       = "org.apache.hadoop"         %  "hadoop-hdfs"              % V.hadoop		 % "provided"
    val hadoopCommonTest = "org.apache.hadoop"         %  "hadoop-common"            % V.hadoop		 % "test"
    val hadoopHdfsTest   = "org.apache.hadoop"         %  "hadoop-hdfs"              % V.hadoop		 % "test"
    // Scala (test only)
    val specs2           = "org.specs2"                 %% "specs2"                  % V.specs2          % "test"
    val scalazSpecs2     = "org.typelevel"              %% "scalaz-specs2"           % V.scalazSpecs2    % "test"
    val mockito          = "org.mockito"                % "mockito-core"             % V.mockito         % "test"
  }
}
