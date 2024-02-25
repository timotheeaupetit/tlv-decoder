ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.3.1"

lazy val root = (project in file("."))
  .settings(
    name := "tlv-decoder",
    libraryDependencies += "org.scodec" %% "scodec-bits" % "1.1.38",
    libraryDependencies += "org.scalatest" %% "scalatest-funspec" % "3.2.18" % Test
  )
