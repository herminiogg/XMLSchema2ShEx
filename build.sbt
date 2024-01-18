ThisBuild / organization := "com.herminiogarcia"

lazy val xmlschema2shex = project
  .in(file("."))
  .settings(
    name := "xmlschema2shex",
    version := "0.1.0",
    scalaVersion := "3.2.0",
    crossScalaVersions := Seq("2.12.17", "2.13.9", "3.2.0"),
    libraryDependencies ++= Seq(
      "org.scala-lang.modules" %% "scala-parser-combinators" % "2.3.0",
      "org.scalatest" %% "scalatest" % "3.2.15" % "test",
      "com.herminiogarcia" %% "shexml" % "0.4.2" exclude("io.gatling", "gatling-jsonpath"),
      "info.picocli" % "picocli" % "4.7.3",
    )
  )