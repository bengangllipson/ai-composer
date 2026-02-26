import java.net.URI

ThisBuild / version := "0.1.0"
ThisBuild / scalaVersion := "3.3.7"
ThisBuild / publishMavenStyle := true
ThisBuild / pomIncludeRepository := { _ => false }

ThisBuild / publishTo := {
  val centralSnapshots = "https://central.sonatype.com/repository/maven-snapshots/"
  if (isSnapshot.value)
    Some("central-snapshots" at centralSnapshots)
  else
    localStaging.value
}

ThisBuild / organization := "com.bengangllipson"
ThisBuild / organizationName := "bengangllipson"
ThisBuild / organizationHomepage := Some(url("https://bengangllipson.com"))
ThisBuild / homepage := Some(url("https://bengangllipson.com"))
ThisBuild / scmInfo := Some(ScmInfo(url("https://github.com/bengangllipson/ai-composer"), "scm:git:git@github.com:bengangllipson/ai-composer.git"))
ThisBuild / licenses := List(
  "GPL-3.0-only" -> URI.create("https://www.gnu.org/licenses/gpl-3.0.txt").toURL
)
ThisBuild / developers := List(Developer("bengangllipson", "Ben Gangl-Lipson", "dev@bengangllipson.com", url("https://bengangllipson.com")))

Compile / packageSrc / publishArtifact := true
Compile / packageDoc / publishArtifact := true
