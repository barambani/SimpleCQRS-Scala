lazy val prjcSettings = Seq (
	scalaVersion := "2.11.7",
	version := "0.0.1",
	name := "CommandSide"
)

lazy val commandSide = (project in file("CommandSide"))
	.settings(prjcSettings: _*)