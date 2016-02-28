lazy val prjcSettings = Seq (
	scalaVersion := "2.11.7",
	version := "0.0.1",
	name := "QuerySide"
)

lazy val querySide = (project in file("QuerySide"))
	.settings(prjcSettings: _*)