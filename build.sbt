name := "ednParser"

version := "1.0"

scalaVersion := "2.11.7"

libraryDependencies ++= Seq(
  "org.clojure" % "clojure" % "1.7.0",
  "org.scalatest" % "scalatest_2.11" % "2.2.4" % "test",
  "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.4"
)