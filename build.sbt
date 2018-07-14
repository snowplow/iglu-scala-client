/*
 * Copyright (c) 2014-2017 Snowplow Analytics Ltd. All rights reserved.
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
    version := "0.5.0",
    description := "Scala client and resolver for Iglu schema repositories"
  )
  .settings(BuildSettings.buildSettings)
  .settings(BuildSettings.publishSettings)
  .settings(BuildSettings.mavenCentralExtras)
  .settings(
    libraryDependencies ++= Seq(
      // Java
      Dependencies.Libraries.commonsLang,
      Dependencies.Libraries.jacksonDatabind,
      Dependencies.Libraries.jsonValidator,
      // Scala
      Dependencies.Libraries.json4sJackson,
      Dependencies.Libraries.json4sScalaz,
      Dependencies.Libraries.scalaz7,
      Dependencies.Libraries.collUtils,
      // Scala (test only)
      Dependencies.Libraries.specs2,
      Dependencies.Libraries.scalazSpecs2,
      Dependencies.Libraries.mockito
    ),
    scalafmtOnCompile := true
  )
