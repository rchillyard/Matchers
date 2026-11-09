organization := "com.phasmidsoftware"

name := "Matchers"

version := "1.1.1"

// The current Scala 3 LTS. Code built with 3.9 can only be used from Scala 3.9 or later; Scala 3.3 users
// should use 1.0.17, which was built with 3.3.8 and is functionally identical (1.1.0 changed only the build).
scalaVersion := "3.9.0"

// The compiler options that all the Phasmid libraries share.
scalacOptions ++= Seq(
  "-encoding", "UTF-8",
  "-deprecation",
  "-feature",
  "-unchecked",
  "-Wunused:all",
  "-Wvalue-discard",
  "-Wnonunit-statement",
  "-Werror"
)
// Tests may use a value as a statement (for example, in a matcher), so they skip -Wnonunit-statement.
Test / scalacOptions ~= (_.filterNot(_ == "-Wnonunit-statement"))

// MiMa checks the published artifact for binary compatibility with the last release: `sbt mimaReportBinaryIssues`.
// The version scheme is PVP (see publish.sbt): a patch release must pass; a minor release may break compatibility,
// on purpose and with a release note, by listing the breaks in mimaBinaryIssueFilters (or, after the release, by
// moving previousRelease on). Note moduleName, not name: the published artifact is matchers_3, not Matchers_3.
val previousRelease = "1.1.0"
mimaPreviousArtifacts := Set(organization.value %% moduleName.value % previousRelease)

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
