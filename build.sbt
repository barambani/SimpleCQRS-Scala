lazy val commonSettings = Seq (
  version := "0.0.1",
  scalaVersion := "2.11.7",
  name := "SimpleCQRS-Scala"
)

lazy val root = (project in file(".")).settings(commonSettings: _*)