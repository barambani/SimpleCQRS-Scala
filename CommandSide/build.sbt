lazy val prjcSettings = Seq (
	scalaVersion := "2.11.7",
	version := "0.0.1",
	name := "CommandSide"
)

lazy val commandSide = (project in file(".")).settings(prjcSettings: _*)

libraryDependencies ++= Seq(
	"org.scalaz" %% "scalaz-core" % "7.2.1",
    "org.specs2" %% "specs2-core" % "3.7.2" % "test"
)

scalacOptions in Test ++= Seq("-Yrangepos")

scalacOptions ++= Seq (
	"-feature",
	"-deprecation",
	"-target:jvm-1.8"
)