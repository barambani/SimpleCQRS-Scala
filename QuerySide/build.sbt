inThisBuild(Seq(
  scalaOrganization := "org.typelevel",
  scalaVersion := "2.12.2-bin-typelevel-4"
))

lazy val prjcSettings = Seq (
	version := "0.0.1",
	name := "QuerySide"
)

lazy val querySide = (project in file(".")).settings(prjcSettings: _*)

scalacOptions ++= Seq (
	"-feature",
	"-deprecation",
	"-target:jvm-1.8"
)