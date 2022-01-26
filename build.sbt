import Dependencies._

ThisBuild / scalaVersion     := "2.13.7"
ThisBuild / version          := "0.1.0-SNAPSHOT"
ThisBuild / organization     := "com.example"
ThisBuild / organizationName := "scala-elm-stblib"

lazy val root = (project in file("."))
  .settings(
    name := "scala-elm-stdlib",
    libraryDependencies += scalaTest % Test,
    libraryDependencies += "org.typelevel" %% "cats-core" % "2.3.0",
    libraryDependencies += "org.typelevel" %% "cats-effect" % "2.5.3",
  )

//calacOptions += "-Ypartial-unification"

// See https://www.scala-sbt.org/1.x/docs/Using-Sonatype.html for instructions on how to publish to Sonatype.
