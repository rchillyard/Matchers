organization := "com.phasmidsoftware"

name := "Matchers"

version := "1.0.17"

scalaVersion := "3.9.0" // the current LTS

scalacOptions ++= Seq("-encoding", "UTF-8", "-unchecked", "-deprecation", "-feature", "-Wunused:all")

val scalaTestVersion = "3.2.20"

libraryDependencies ++= Seq(
    "org.slf4j" % "slf4j-api" % "2.0.18",
    "org.scalatest" %% "scalatest" % scalaTestVersion % "test",
    "ch.qos.logback" % "logback-classic" % "1.5.38" % "test"
)

resolvers += "Typesafe Repository" at "https://repo.typesafe.com/typesafe/releases/"

// JUnit-style reports, collected by CircleCI
Test / testOptions += Tests.Argument(TestFrameworks.ScalaTest, "-u", "target/test-reports")

// Scaladoc is published to https://rchillyard.github.io/Matchers/latest/api/ by `sbt ghpagesPushSite`
enablePlugins(GhpagesPlugin, SiteScaladocPlugin)
git.remoteRepo := "git@github.com:rchillyard/Matchers.git"
