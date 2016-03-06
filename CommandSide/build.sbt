lazy val prjcSettings = Seq (
	scalaVersion := "2.11.7",
	version := "0.0.1",
	name := "CommandSide"
)

lazy val commandSide = (project in file("."))
	.settings(prjcSettings: _*)

libraryDependencies ++= Seq(
    "org.specs2" %% "specs2" % "3.0.1" % "test",
    "org.specs2" %% "specs2-junit" % "3.0.1" % "test"
)

resolvers += "scalaz-bintray" at "http://dl.bintray.com/scalaz/releases"