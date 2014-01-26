organization := "ie.boboco"

name := "Bridge"

version := "1.1"

scalaVersion := "2.10.3"

libraryDependencies ++= Seq(
    // see https://github.com/scalatest/scalatest/issues/181
    // for why we are using 2.0.1-SNAP instead of 2.0
    "org.scalatest" %% "scalatest" % "2.0.1-SNAP" % "test",
    "org.scalacheck" %% "scalacheck" % "1.11.3" % "test"
)

scalacOptions ++= Seq("-unchecked", "-deprecation")
