organization := "com.phasmidsoftware"

name := "Matchers"

version := "1.0.4-SNAPSHOT"

scalaVersion := "2.13.6"

scalacOptions ++= Seq( "-target:jvm-1.8", "-encoding", "UTF-8", "-unchecked", "-deprecation", "-Ywarn-dead-code", "-Ywarn-value-discard", "-Ywarn-unused" )

val scalaTestVersion = "3.2.9"

libraryDependencies += "org.slf4j" % "slf4j-api" % "1.7.31"

libraryDependencies += "org.scalatest" %% "scalatest" % scalaTestVersion % "test"

libraryDependencies +="ch.qos.logback" % "logback-classic" % "1.2.3" % "test"

resolvers += "Typesafe Repository" at "https://repo.typesafe.com/typesafe/releases/"

libraryDependencies += "org.scala-lang" % "scala-compiler" % scalaVersion.value

