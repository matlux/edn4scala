name := "edn4scala"

organization := "net.cabworks"

version := "0.1"

scalaVersion := "2.11.7"

libraryDependencies ++= Seq(
  "org.clojure" % "clojure" % "1.7.0",
  "org.scalatest" %% "scalatest" % "2.2.4" % "test",
  "org.scalacheck" %% "scalacheck" % "1.12.5" % "test"
)

publishTo := {
  val nexus = "https://FIND_ME_A_NEXUS_REPO/"
  if (isSnapshot.value)
    Some("snapshots" at nexus + "content/repositories/snapshots")
  else
    Some("releases"  at nexus + "service/local/staging/deploy/maven2")
}

credentials += Credentials("Some Nexus Repository Manager", "FIND_ME_A_NEXUS_REPO", "admin", "admin123")