lazy val prjcSettings = Seq (
	scalaVersion := "2.11.7",
	version := "0.0.1",
	name := "QuerySide"
)

lazy val querySide = (project in file(".")).settings(prjcSettings: _*)

libraryDependencies ++= Seq(
    "org.scalaz" %% "scalaz-core" % "7.2.1",
    "org.specs2" %% "specs2-core" % "3.7.2" % "test"
)

scalacOptions in Test ++= Seq("-Yrangepos")