lazy val commonSettings = Seq (
	scalaVersion := "2.11.7",
	version := "0.0.1",
	name := "SimpleCqrsScala"
)

lazy val commandSide = project in file("CommandSide")
lazy val querySide = project in file("QuerySide")

lazy val simpleCqrsScala = (project in file("."))
	.aggregate(commandSide, querySide)
	.dependsOn(commandSide, querySide)
	.settings(commonSettings: _*)

initialCommands in console := """
	|import java.util.UUID
    |import scalaz._
    |import Scalaz._
    |import SimpleCqrsScala.CommandSide._
    |import SimpleCqrsScala.CommandSide.Domain._
""".stripMargin

logLevel := Level.Info