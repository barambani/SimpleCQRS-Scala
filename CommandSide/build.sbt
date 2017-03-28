import sbt._

lazy val prjcSettings = Seq (
	scalaVersion := "2.12.1",
	scalaOrganization in ThisBuild := "org.typelevel",
	version := "0.0.1",
	name := "CommandSide"
)

lazy val commandSide = (project in file(".")).settings(prjcSettings: _*)

resolvers += Resolver.sonatypeRepo("releases")
resolvers += Resolver.sonatypeRepo("snapshots")

val monocleVersion = "1.4.0"

libraryDependencies ++= Seq(
	"org.scalaz" % "scalaz-core_2.12" % "7.3.0-M10",
	"org.scalaz" % "scalaz-concurrent_2.12" % "7.3.0-M10",
	"co.fs2" % "fs2-core_2.12" % "0.9.4",
	"co.fs2" % "fs2-io_2.12" % "0.9.4",

    "com.github.julien-truffaut"  %%  "monocle-core"    % monocleVersion,
  	"com.github.julien-truffaut"  %%  "monocle-generic" % monocleVersion,
  	"com.github.julien-truffaut"  %%  "monocle-macro"   % monocleVersion,        
  	"com.github.julien-truffaut"  %%  "monocle-state"   % monocleVersion,     
  	"com.github.julien-truffaut"  %%  "monocle-refined" % monocleVersion,
	"com.github.julien-truffaut"  %%  "monocle-law"     % monocleVersion % "test",

	"org.specs2" % "specs2-core_2.12" % "3.8.9" % "test",

	"org.scalacheck" % "scalacheck_2.12" % "1.13.5" % "test"
)

scalacOptions in Test ++= Seq("-Yrangepos")

scalacOptions ++= Seq (
	"-feature",
	"-deprecation",
	"-target:jvm-1.8"
)

addCompilerPlugin("org.scalamacros" %% "paradise" % "2.1.0" cross CrossVersion.full)
