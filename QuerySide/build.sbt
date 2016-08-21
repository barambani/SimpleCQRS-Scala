lazy val prjcSettings = Seq (
	scalaVersion := "2.11.8",
	version := "0.0.1",
	name := "QuerySide"
)

lazy val querySide = (project in file(".")).settings(prjcSettings: _*)

libraryDependencies ++= Seq(
    "org.scalaz" %% "scalaz-core" % "7.2.5",
    "org.specs2" %% "specs2-core" % "3.8.4" % "test"
)

scalacOptions in Test ++= Seq("-Yrangepos")

scalacOptions ++= Seq (
	"-feature",
	"-deprecation",
	"-target:jvm-1.8"
)