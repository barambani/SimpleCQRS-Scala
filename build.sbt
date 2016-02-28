lazy val commonSettings = Seq (
	scalaVersion := "2.11.7",
	version := "0.0.1",
	name := "SimpleCQRS-Scala"
)

lazy val commandSide = project in file("CommandSide")
lazy val querySide = project in file("QuerySide")

lazy val simpleCqrsScala = (project in file("."))
	.aggregate(commandSide, querySide)
	.settings(commonSettings: _*)