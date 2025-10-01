organization := "com.phasmidsoftware"

name := "Matchers"

version := "1.0.11"

//scalaVersion := "2.13.16"
scalaVersion := "3.7.3"

scalacOptions ++= Seq("-encoding", "UTF-8", "-unchecked", "-deprecation" )

val scalaTestVersion = "3.2.19"

libraryDependencies ++= Seq(
    "org.slf4j" % "slf4j-api" % "2.0.17",
    "org.scalatest" %% "scalatest" % scalaTestVersion % "test",
    "ch.qos.logback" % "logback-classic" % "1.5.18" % "test"
)

libraryDependencies += "org.slf4j" % "slf4j-api" % "2.0.17"

resolvers += "Typesafe Repository" at "https://repo.typesafe.com/typesafe/releases/"

//libraryDependencies += "org.scala-lang" % "scala-compiler" % scalaVersion.value

