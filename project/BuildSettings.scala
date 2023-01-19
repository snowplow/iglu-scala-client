/*
 * Copyright (c) 2014-2023 Snowplow Analytics Ltd. All rights reserved.
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

// SBT
import sbt.Keys._
import sbt._

// dynver plugin
import sbtdynver.DynVerPlugin.autoImport._

// Mima plugin
import com.typesafe.tools.mima.plugin.MimaKeys._

// Scoverage plugin
import scoverage.ScoverageKeys._

// Site plugin
import com.typesafe.sbt.site.SitePlugin.autoImport.siteSubdirName
import com.typesafe.sbt.site.SiteScaladocPlugin.autoImport._

object BuildSettings {

  lazy val buildSettings = Seq[Setting[_]](
    organization := "com.snowplowanalytics",
    scalaVersion := "2.13.9",
    crossScalaVersions := Seq("3.2.0", "2.13.9", "2.12.17"),
    licenses += ("Apache-2.0", url("http://www.apache.org/licenses/LICENSE-2.0.html")),
    Test / parallelExecution := false, // possible race bugs
    libraryDependencies ++= {
      if (scalaBinaryVersion.value.startsWith("2")) {
        List(
          compilerPlugin("org.typelevel" %% "kind-projector" % "0.13.2" cross CrossVersion.full),
          compilerPlugin("com.olegpy" %% "better-monadic-for" % "0.3.1")
        )
      } else Nil
    }
 )


  // Bintray publishing settings
  lazy val publishSettings = Seq[Setting[_]](
    publishArtifact := true,
    Test / publishArtifact := false,
    pomIncludeRepository := { _ => false },
    homepage := Some(url("http://snowplowanalytics.com")),
    ThisBuild / dynverVTagPrefix := false, // Otherwise git tags required to have v-prefix
    developers := List(
      Developer(
        "Snowplow Analytics Ltd",
        "Snowplow Analytics Ltd",
        "support@snowplowanalytics.com",
        url("https://snowplowanalytics.com")
      )
    )
  )

  // If new version introduces breaking changes,
  // clear-out mimaBinaryIssueFilters and mimaPreviousVersions.
  // Otherwise, add previous version to set without
  // removing other versions.
  val mimaPreviousVersionsData = Set("2.1.0", "2.2.0")
  val mimaPreviousVersionsCore = Set("2.1.0", "2.2.0")
  val mimaPreviousVersionsHttp4s = Set("2.0.0", "2.1.0", "2.2.0")
  val mimaPreviousVersionsScala3 = Set()

  lazy val mimaSettings = Seq(
    mimaPreviousArtifacts := {
      val mimaPreviousVersions = 
        (name.value, CrossVersion.partialVersion(scalaVersion.value)) match {
          case (_, Some((3, _))) =>
            mimaPreviousVersionsScala3
          case (name, _) if name.endsWith("http4s") =>
            mimaPreviousVersionsHttp4s
          case (name, _) if name.endsWith("data") =>
            mimaPreviousVersionsData
          case _ =>
            mimaPreviousVersionsCore
        }
          
      mimaPreviousVersions.map { organization.value %% name.value % _ }
    },
    ThisBuild / mimaFailOnNoPrevious := false,
    mimaBinaryIssueFilters ++= Seq(),
    Test / test := {
      mimaReportBinaryIssues.value
      (Test / test).value
    }
  )

  val scoverageSettings = Seq(
    coverageMinimumStmtTotal := 50,
    coverageFailOnMinimum := false,
    coverageHighlighting := false,
    (Test / test) := {
      (coverageReport dependsOn (Test / test)).value
    }
  )

  val docsSettings = Seq(
    SiteScaladoc / siteSubdirName := s"${version.value}",
  )
}
