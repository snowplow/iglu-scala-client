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

lazy val root = (project in file("."))
  .settings(
    name := "iglu-scala-client",
    version := "0.6.0-M9",
    description := "Scala client and resolver for Iglu schema repositories"
  )
  .settings(BuildSettings.buildSettings)
  .settings(BuildSettings.publishSettings)
  .settings(BuildSettings.mavenCentralExtras)
  .settings(
    libraryDependencies ++= Seq(
      // Java
      Dependencies.Libraries.jsonValidator,
      // Scala
      Dependencies.Libraries.igluCore,
      Dependencies.Libraries.igluCoreCirce,
      Dependencies.Libraries.cats,
      Dependencies.Libraries.circeTime,
      Dependencies.Libraries.circeParser,
      Dependencies.Libraries.lruMap,
      Dependencies.Libraries.scalaj,
      // Scala (test only)
      Dependencies.Libraries.circeLiteral,
      Dependencies.Libraries.specs2,
      Dependencies.Libraries.specs2Cats,
      // Java (test only)
      Dependencies.Libraries.slf4jNop
    ),
    scalafmtOnCompile := true
  )
