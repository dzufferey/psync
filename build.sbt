name := "psync"

organization := "com.github.dzufferey"

version := "0.3-SNAPSHOT"

scalaVersion := "2.13.1"

scalacOptions in Compile ++= Seq(
    "-unchecked",
    "-deprecation",
    "-feature",
    "-language:implicitConversions",
//    "-Ymacro-debug-lite"
//    "-Xlog-implicits"
//    "-Xlog-implicit-conversions"
)

logBuffered in Test := false

parallelExecution in Test := false

resolvers += "jitpack" at "https://jitpack.io"

libraryDependencies ++=  Seq(
    "org.scala-lang" % "scala-reflect" % scalaVersion.value,
    "org.scala-lang" % "scala-compiler" % scalaVersion.value,
    "org.scala-lang.modules" %% "scala-xml" % "1.3.0",
    "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.2",
    "org.scalatest" %% "scalatest" % "3.2.2" % "test",
    "org.scalacheck" %% "scalacheck" % "1.14.1" % "test",
    "io.netty" % "netty-all" % "4.1.58.Final",
    "com.twitter" %% "chill" % "0.9.5",
    "ch.qos.logback" % "logback-classic" % "1.2.3",
    "com.github.dzufferey" %% "scala-arg" % "1.0.0",
    "com.github.dzufferey" %% "report" % "1.0.0",
    "com.github.dzufferey" %% "misc-scala-utils" % "1.0.0",
    "com.github.dzufferey" %% "scala-smtlib-interface" % "1.0.0"
)

