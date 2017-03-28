lazy val prjcSettings = Seq (
	scalaVersion := "2.12.1",
	scalaOrganization in ThisBuild := "org.typelevel",
	version := "0.0.1",
	name := "QuerySide"
)

lazy val querySide = (project in file(".")).settings(prjcSettings: _*)

scalacOptions ++= Seq (
	"-feature",
	"-deprecation",
	"-target:jvm-1.8"
)