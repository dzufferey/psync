name := "psync"

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

libraryDependencies ++=  Seq(
    "org.scala-lang" % "scala-reflect" % scalaVersion.value,
    "org.scala-lang" % "scala-compiler" % scalaVersion.value,
    "org.scala-lang.modules" %% "scala-xml" % "1.2.0",
    "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.2",
    "org.scalatest" %% "scalatest" % "3.1.1" % "test",
    "org.scalacheck" %% "scalacheck" % "1.14.1" % "test",
    "io.netty" % "netty-all" % "4.1.48.Final",
    "com.twitter" %% "chill" % "0.9.5",
    "ch.qos.logback" % "logback-classic" % "1.2.3",
    "io.github.dzufferey" %% "scala-arg" % "0.1-SNAPSHOT",
    "io.github.dzufferey" %% "report" % "0.1-SNAPSHOT",
    "io.github.dzufferey" %% "misc-scala-utils" % "0.1-SNAPSHOT",
    "io.github.dzufferey" %% "scala-smtlib-interface" % "0.1-SNAPSHOT"
)

//addCompilerPlugin("org.psywerx.hairyfotr" %% "linter" % "0.1.17")

resolvers += Resolver.sonatypeRepo("snapshots")

resolvers +=  "dzufferey maven repo" at "https://github.com/dzufferey/my_mvn_repo/raw/master/repository"

