organization := "com.phasmidsoftware"

name := "Matchers"

version := "1.0.7"

scalaVersion := "2.13.16"

scalacOptions ++= Seq( "-encoding", "UTF-8", "-unchecked", "-deprecation", "-Ywarn-dead-code", "-Ywarn-value-discard", "-Ywarn-unused" )

val scalaTestVersion = "3.2.19"

libraryDependencies += "org.slf4j" % "slf4j-api" % "2.0.16"

libraryDependencies += "org.scalatest" %% "scalatest" % scalaTestVersion % "test"

libraryDependencies +="ch.qos.logback" % "logback-classic" % "1.5.16" % "test"

resolvers += "Typesafe Repository" at "https://repo.typesafe.com/typesafe/releases/"

libraryDependencies += "org.scala-lang" % "scala-compiler" % scalaVersion.value

