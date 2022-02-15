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

lazy val core = (project in file("modules/core"))
  .settings(
    name := "iglu-scala-client",
    description := "Scala client and resolver for Iglu schema repositories"
  )
  .enablePlugins(SiteScaladocPlugin, GhpagesPlugin, PreprocessPlugin)
  .settings(BuildSettings.buildSettings)
  .settings(BuildSettings.publishSettings)
  .settings(BuildSettings.mimaSettings)
  .settings(BuildSettings.scoverageSettings)
  .settings(BuildSettings.ghPagesSettings)
  .settings(
    libraryDependencies ++= Seq(
      // Java
      Dependencies.Libraries.validator,
      Dependencies.Libraries.jackson,
      // Scala
      Dependencies.Libraries.igluCore,
      Dependencies.Libraries.igluCoreCirce,
      Dependencies.Libraries.cats,
      Dependencies.Libraries.catsEffect,
      Dependencies.Libraries.circeParser,
      Dependencies.Libraries.lruMap,
      Dependencies.Libraries.scalaj,
      Dependencies.Libraries.collectionCompat,
      // Scala (test only)
      Dependencies.Libraries.circeLiteral,
      Dependencies.Libraries.specs2,
      Dependencies.Libraries.specs2Cats,
      Dependencies.Libraries.specs2CatsEffect,
      // Java (test only)
      Dependencies.Libraries.slf4jNop
    )
  )

lazy val http4s = (project in file("modules/http4s"))
  .settings(
    name := "iglu-scala-client-http4s",
    description := "Resolver for Iglu schema repositories backed by a http4s client"
  )
  .settings(BuildSettings.buildSettings)
  .settings(BuildSettings.publishSettings)
  .settings(BuildSettings.mimaSettings)
  .settings(BuildSettings.scoverageSettings)
  .settings(
    libraryDependencies ++= Seq(
      Dependencies.Libraries.http4sClient,
      Dependencies.Libraries.http4sCirce,
      // Scala (test only)
      Dependencies.Libraries.circeCore,
      Dependencies.Libraries.specs2,
      Dependencies.Libraries.specs2CatsEffect,
      Dependencies.Libraries.http4sDsl
    )
  )
  .dependsOn(core)
