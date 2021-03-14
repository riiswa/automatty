val scala3Version = "3.0.0-RC1"

lazy val root = project
  .in(file("."))
  .settings(
    name := "automatty",
    version := "0.1.0",

    scalaVersion := scala3Version,

    // https://mvnrepository.com/artifact/guru.nidi/graphviz-java
    libraryDependencies += "guru.nidi" % "graphviz-java" % "0.18.0",

    libraryDependencies += "org.scalactic" %% "scalactic" % "3.2.5",
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.5" % "test"

  )
