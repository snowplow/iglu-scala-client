addSbtPlugin("io.github.davidgregory084" % "sbt-tpolecat"    % "0.4.1")
addSbtPlugin("org.scalameta"             % "sbt-scalafmt"    % "2.4.6")
addSbtPlugin("com.typesafe"              % "sbt-mima-plugin" % "1.1.1")
addSbtPlugin("org.scoverage"             % "sbt-scoverage"   % "2.4.0")
addSbtPlugin("com.typesafe.sbt"          % "sbt-site"        % "1.4.1")
addSbtPlugin("org.scoverage"             % "sbt-coveralls"   % "1.3.2")
addSbtPlugin("com.github.sbt"            % "sbt-ci-release"  % "1.11.2")
addSbtPlugin("pl.project13.scala"        % "sbt-jmh"         % "0.4.8")

libraryDependencySchemes += "org.scala-lang.modules" %% "scala-xml" % VersionScheme.Always
