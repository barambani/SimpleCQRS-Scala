import sbt._

lazy val prjcSettings = Seq (
	scalaVersion := "2.12.1",
	version := "0.0.1",
	name := "CommandSide"
)

lazy val commandSide = (project in file(".")).settings(prjcSettings: _*)

resolvers += Resolver.sonatypeRepo("releases")
resolvers += Resolver.sonatypeRepo("snapshots")

val monocleVersion = "1.4.0"

libraryDependencies ++= Seq(
	"org.scalaz" %% "scalaz-core" % "7.2.8",

    "com.github.julien-truffaut"  %%  "monocle-core"    % monocleVersion,
  	"com.github.julien-truffaut"  %%  "monocle-generic" % monocleVersion,
  	"com.github.julien-truffaut"  %%  "monocle-macro"   % monocleVersion,        
  	"com.github.julien-truffaut"  %%  "monocle-state"   % monocleVersion,     
  	"com.github.julien-truffaut"  %%  "monocle-refined" % monocleVersion,
	"com.github.julien-truffaut"  %%  "monocle-law"     % monocleVersion % "test",

	"org.specs2" %% "specs2-core" % "3.8.6" % "test"
)

scalacOptions in Test ++= Seq("-Yrangepos")

scalacOptions ++= Seq (
	"-feature",
	"-deprecation",
	"-target:jvm-1.8"
)

addCompilerPlugin("org.scalamacros" %% "paradise" % "2.1.0" cross CrossVersion.full)
