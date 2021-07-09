/*
 * Copyright (c) 2014-2020 Snowplow Analytics Ltd. All rights reserved.
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
import sbt._
import Keys._

// dynver plugin
import sbtdynver.DynVerPlugin.autoImport._

// Mima plugin
import com.typesafe.tools.mima.plugin.MimaKeys._
import com.typesafe.tools.mima.plugin.MimaPlugin

// Scoverage plugin
import scoverage.ScoverageKeys._

// GHPages plugin
import com.typesafe.sbt.sbtghpages.GhpagesPlugin.autoImport._
import com.typesafe.sbt.site.SitePlugin.autoImport.{makeSite, siteSubdirName}
import com.typesafe.sbt.SbtGit.GitKeys.{gitBranch, gitRemoteRepo}
import com.typesafe.sbt.site.SiteScaladocPlugin.autoImport._
import com.typesafe.sbt.site.preprocess.PreprocessPlugin.autoImport._

object BuildSettings {

  lazy val buildSettings = Seq[Setting[_]](
    organization := "com.snowplowanalytics",
    scalaVersion := "2.12.14",
    crossScalaVersions := Seq("2.12.14", "2.13.6"),
    licenses += ("Apache-2.0", url("http://www.apache.org/licenses/LICENSE-2.0.html")),

    addCompilerPlugin("org.typelevel" %% "kind-projector" % "0.13.0" cross CrossVersion.full),

    addCompilerPlugin("com.olegpy" %% "better-monadic-for" % "0.3.0"),

    Test / parallelExecution := false // possible race bugs
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
  val mimaPreviousVersionsCore = Set("1.0.2")
  val mimaPreviousVersionsHttp4s = Set()

  lazy val mimaSettings = Seq(
    mimaPreviousArtifacts := {
      val mimaPreviousVersions = if (name.value.endsWith("http4s")) mimaPreviousVersionsHttp4s else mimaPreviousVersionsCore
      mimaPreviousVersions.map { organization.value %% name.value % _ },
    },
    ThisBuild / mimaFailOnNoPrevious := false,
    mimaBinaryIssueFilters ++= Seq(),
    Test / test := {
      mimaReportBinaryIssues.value
      (Test / test).value
    }
  )

  val scoverageSettings = Seq(
    coverageMinimum := 50,
    coverageFailOnMinimum := false,
    coverageHighlighting := false,
    (Test / test) := {
      (coverageReport dependsOn (Test / test)).value
    }
  )

  val ghPagesSettings = Seq(
    ghpagesPushSite := (ghpagesPushSite dependsOn makeSite).value,
    ghpagesNoJekyll := false,
    gitRemoteRepo := "git@github.com:snowplow/iglu-scala-client.git",
    gitBranch := Some("gh-pages"),
    SiteScaladoc / siteSubdirName := s"${version.value}",
    Preprocess / preprocessVars := Map("VERSION" -> version.value),
    ghpagesCleanSite / excludeFilter := new FileFilter {
      def accept(f: File) = true
    }
  )
}
