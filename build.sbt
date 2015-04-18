name := "round"

version := "0.1-SNAPSHOT"

scalaVersion := "2.11.6"

scalacOptions in Compile ++= Seq(
    "-unchecked",
    "-deprecation",
    "-feature",
    "-language:implicitConversions",
    "-Xmax-classfile-name", "110"//,
//    "-Ymacro-debug-lite"
//    "-Xlog-implicits"
//    "-Xlog-implicit-conversions"
)

libraryDependencies ++=  Seq(
    "org.scala-lang" % "scala-reflect" % scalaVersion.value,
    "org.scala-lang" % "scala-compiler" % scalaVersion.value,
    "org.scala-lang.modules" %% "scala-xml" % "1.0.2",
    "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.3",
    "org.scalatest" %% "scalatest" % "2.2.1" % "test",
    "org.scalacheck" %% "scalacheck" % "1.11.6" % "test",
    "org.scala-lang.modules" %% "scala-pickling" % "0.10.0",
    "io.netty" % "netty-all" % "4.0.27.Final",
    "io.github.dzufferey" %% "scala-arg" % "0.1-SNAPSHOT",
    "io.github.dzufferey" %% "report" % "0.1-SNAPSHOT",
    "io.github.dzufferey" %% "misc-scala-utils" % "0.1-SNAPSHOT"
)

resolvers += Resolver.sonatypeRepo("snapshots")

resolvers +=  "dzufferey maven repo" at "https://github.com/dzufferey/my_mvn_repo/raw/master/repository"

