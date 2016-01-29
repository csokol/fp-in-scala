name := "fp-in-scala"
organization := "br.com.sokol"
version := "1.0.0-SNAPSHOT"

scalaVersion := "2.11.7"

parallelExecution in ThisBuild := false

libraryDependencies ++= Seq(
  "org.mockito" % "mockito-core" % "1.9.5" % "test",
  "org.scalatest" %% "scalatest" % "2.2.3" % "test",
  "org.specs2" %% "specs2" % "2.3.12" % "test"
)

