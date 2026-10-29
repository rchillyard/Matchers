organization := "com.phasmidsoftware"

name := "Matchers"

version := "1.0.17"

scalaVersion := "3.3.8" // LTS version

scalacOptions ++= Seq("-encoding", "UTF-8", "-unchecked", "-deprecation" )

val scalaTestVersion = "3.2.20"

libraryDependencies ++= Seq(
    "org.slf4j" % "slf4j-api" % "2.0.18",
    "org.scalatest" %% "scalatest" % scalaTestVersion % "test",
    "ch.qos.logback" % "logback-classic" % "1.5.38" % "test"
)

resolvers += "Typesafe Repository" at "https://repo.typesafe.com/typesafe/releases/"
