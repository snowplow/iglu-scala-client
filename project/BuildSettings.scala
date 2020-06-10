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

// Bintray plugin
import bintray.BintrayPlugin._
import bintray.BintrayKeys._

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
    scalaVersion := "2.13.2",
    crossScalaVersions := Seq("2.12.8", "2.13.2"),

    addCompilerPlugin("org.typelevel" %% "kind-projector" % "0.11.0" cross CrossVersion.full),

    addCompilerPlugin("com.olegpy" %% "better-monadic-for" % "0.3.0"),

    parallelExecution in Test := false // possible race bugs
  )


  // Bintray publishing settings
  lazy val publishSettings = bintraySettings ++ Seq[Setting[_]](
    licenses += ("Apache-2.0", url("http://www.apache.org/licenses/LICENSE-2.0.html")),
    bintrayOrganization := Some("snowplow"),
    bintrayRepository := "snowplow-maven"
  )

  // Maven Central publishing settings
  lazy val mavenCentralExtras = Seq[Setting[_]](
    pomIncludeRepository := { x => false },
    homepage := Some(url("http://snowplowanalytics.com")),
    scmInfo := Some(ScmInfo(url("https://github.com/snowplow/iglu-scala-client"), "scm:git@github.com:snowplow/iglu-scala-client.git")),
    pomExtra := (
      <developers>
        <developer>
          <name>Snowplow Analytics Ltd</name>
          <email>support@snowplowanalytics.com</email>
          <organization>Snowplow Analytics Ltd</organization>
          <organizationUrl>http://snowplowanalytics.com</organizationUrl>
        </developer>
      </developers>)
  )

  // If new version introduces breaking changes,
  // clear-out mimaBinaryIssueFilters and mimaPreviousVersions.
  // Otherwise, add previous version to set without
  // removing other versions.
  val mimaPreviousVersions = Set()

  val mimaSettings = MimaPlugin.mimaDefaultSettings ++ Seq(
    mimaPreviousArtifacts := mimaPreviousVersions.map { organization.value %% name.value % _ },
    mimaBinaryIssueFilters ++= Seq(),
    test in Test := {
      mimaReportBinaryIssues.value
      (test in Test).value
    }
  )

  val scoverageSettings = Seq(
    coverageMinimum := 50,
    coverageFailOnMinimum := false,
    coverageHighlighting := false,
    (test in Test) := {
      (coverageReport dependsOn (test in Test)).value
    }
  )

  val ghPagesSettings = Seq(
    ghpagesPushSite := (ghpagesPushSite dependsOn makeSite).value,
    ghpagesNoJekyll := false,
    gitRemoteRepo := "git@github.com:snowplow/iglu-scala-client.git",
    gitBranch := Some("gh-pages"),
    siteSubdirName in SiteScaladoc := s"${version.value}",
    preprocessVars in Preprocess := Map("VERSION" -> version.value),
    excludeFilter in ghpagesCleanSite := new FileFilter {
      def accept(f: File) = true
    }
  )
}
