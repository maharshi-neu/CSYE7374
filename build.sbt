scalaVersion := "3.1.0"

lazy val tsecVersion = "0.4.0"

libraryDependencies ++= Seq(
  "io.github.jmcardon" %% "tsec-cipher-jca" % tsecVersion,
  "io.github.jmcardon" %% "tsec-hash-jca" % tsecVersion,
  "org.scala-lang.modules" %% "scala-parser-combinators" % "2.1.1",
  "com.novocode" % "junit-interface" % "0.11" % "test",
  "org.scalatest" %% "scalatest" % "3.2.12" % Test
)