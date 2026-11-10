// Optional but useful
ThisBuild / description := "Scala library of composable matchers"
ThisBuild / homepage := Some(url("https://github.com/rchillyard/Matchers"))
ThisBuild / organizationName := "Phasmid Software"
ThisBuild / organizationHomepage := Some(url("https://phasmidsoftware.com/"))

// Publishing configuration
ThisBuild / publishTo := {
  val centralSnapshots = "https://central.sonatype.com/repository/maven-snapshots/"
  if (isSnapshot.value) Some("central-snapshots" at centralSnapshots)
  else localStaging.value
}
ThisBuild / publishMavenStyle := true

// Credentials
ThisBuild / credentials += Credentials(Path.userHome / ".sbt" / "sonatype_credentials")

// Required POM metadata for Maven Central
ThisBuild / licenses := List("MIT" -> url("https://opensource.org/licenses/MIT"))
ThisBuild / scmInfo := Some(
  ScmInfo(
    url("https://github.com/rchillyard/Matchers"),
    "scm:git@github.com:rchillyard/Matchers.git"
  )
)
ThisBuild / developers := List(
  Developer(
    id    = "rchillyard",
    name  = "Robin Hillyard",
    email = "rchillyard@phasmidsoftware.com",
    url   = url("https://phasmidsoftware.com")
  )
)

// Optional

// Publishing settings
// PVP: the first two numbers make the major version. A patch release (x.y.z) is binary compatible with x.y.0;
// a minor release (x.y) may break compatibility, with a release note. (Up to 1.1.0, this said "early-semver",
// which promised binary compatibility across minor releases, so sbt and Coursier evicted older ones silently.)
ThisBuild / versionScheme := Some("pvp")
ThisBuild / publishConfiguration := publishConfiguration.value.withOverwrite(true)
ThisBuild / publishLocalConfiguration := publishLocalConfiguration.value.withOverwrite(true)


// Remove all additional repository other than Maven Central from POM
ThisBuild / pomIncludeRepository := { _ => false }

// sbt's lint wrongly reports these ThisBuild publish settings as unused
Global / excludeLintKeys ++= Set(pomIncludeRepository, publishMavenStyle)
