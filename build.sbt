organization := "com.phasmidsoftware"

name := "Matchers"

version := "1.1.0"

scalaVersion := "3.9.0" // the current LTS

// The compiler options that all the Phasmid libraries share.
scalacOptions ++= Seq(
  "-encoding", "UTF-8",
  "-deprecation",
  "-feature",
  "-unchecked",
  "-Wunused:all",
  "-Wvalue-discard",
  "-Wnonunit-statement"
)
// Tests may use a value as a statement (for example, in a matcher), so they skip -Wnonunit-statement.
Test / scalacOptions ~= (_.filterNot(_ == "-Wnonunit-statement"))

val scalaTestVersion = "3.2.20"

libraryDependencies ++= Seq(
    "org.slf4j" % "slf4j-api" % "2.0.18",
    "org.scalatest" %% "scalatest" % scalaTestVersion % "test",
    "ch.qos.logback" % "logback-classic" % "1.5.38" % "test"
)

resolvers += "Typesafe Repository" at "https://repo.typesafe.com/typesafe/releases/"


// Scaladoc is published to https://rchillyard.github.io/Matchers/latest/api/ by `sbt ghpagesPushSite`
enablePlugins(GhpagesPlugin, SiteScaladocPlugin)
git.remoteRepo := "git@github.com:rchillyard/Matchers.git"
