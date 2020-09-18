name := "xmlschema2shex"

organization := "es.weso"

version := "0.1-SNAPSHOT"

scalaVersion := "2.12.4"

resolvers += "jitpack" at "https://jitpack.io"

libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.2"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.2" % "test"
libraryDependencies += "com.github.herminiogg" % "shexml" % "master-SNAPSHOT"


