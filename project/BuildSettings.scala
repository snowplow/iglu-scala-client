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
import sbt._
import Keys._

object BuildSettings {

  lazy val buildSettings = Seq[Setting[_]](
    organization  := "com.snowplowanalytics",
    scalaVersion  := "2.10.4",
    crossScalaVersions  := Seq("2.10.4", "2.11.7"),
    scalacOptions := Seq(
      "-deprecation",
      "-encoding", "UTF-8",
      "-feature",
      "-unchecked",
      "-Ywarn-dead-code",
      "-Ywarn-inaccessible",
      "-Ywarn-nullary-override",
      "-Ywarn-nullary-unit",
      "-Ywarn-numeric-widen",
      "-Ywarn-value-discard"
    ),

    scalacOptions in (Compile, console) ~= (_ filterNot (_ == "-Xfatal-warnings")),
    scalacOptions in (Test, console) := (scalacOptions in (Compile, console)).value,

    // force scala version
    // http://stackoverflow.com/questions/27809280/suppress-sbt-eviction-warnings
    ivyScala := ivyScala.value map { _.copy(overrideScalaVersion = true) },

    parallelExecution in Test := false // possible race bugs
  )
}
