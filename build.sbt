name := "Find a good name"

version := "0.1-SNAPSHOT"

organization := "at.ac.ist, edu.mit.csail.pac, at.ac.tuwien.forsyte"

scalaVersion := "2.11.1"

scalacOptions in Compile ++= Seq("-unchecked", "-deprecation")

libraryDependencies ++=  Seq(
    //"org.scala-lang" % "scala-actors" % scalaVersion.value
    "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.1",
    "org.scalatest" % "scalatest_2.11" % "2.1.5" % "test",
    "org.apache.commons" % "commons-lang3" % "3.2.1"
)
