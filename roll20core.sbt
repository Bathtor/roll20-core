enablePlugins(ScalaJSPlugin)
import sbtcrossproject.CrossPlugin.autoImport.{crossProject, CrossType}

name := "Roll20 Core Root"

ThisBuild / organization := "com.lkroll"

ThisBuild / scalaVersion := "2.13.5"
ThisBuild / crossScalaVersions := Seq("2.11.12", "2.12.13", "2.13.5")

ThisBuild / licenses += ("MIT", url("http://opensource.org/licenses/MIT"))
ThisBuild / homepage := Some(url("https://github.com/Bathtor/roll20-core"))
ThisBuild / scmInfo := Some(
                ScmInfo(url("https://github.com/Bathtor/roll20-core"),
                            "git@github.com:Bathtor/roll20-core.git"))
ThisBuild / developers := List(
                    Developer(id = "lkroll",
                             name = "Lars Kroll",
                             email = "bathtor@googlemail.com",
                             url = url("https://github.com/Bathtor")))
ThisBuild / publishMavenStyle := true

// Add sonatype repository settings
sonatypeCredentialHost := "s01.oss.sonatype.org"
sonatypeRepository := "https://s01.oss.sonatype.org/service/local"
publishTo := sonatypePublishToBundle.value

import ReleaseTransformations._
releaseUseGlobalVersion := false
releaseCrossBuild := true
publish / skip := true
releaseProcess := Seq[ReleaseStep](
  checkSnapshotDependencies,
  inquireVersions,
  runClean,
  runTest,
  setReleaseVersion,
  commitReleaseVersion,
  tagRelease,
  releaseStepCommandAndRemaining("+publishSigned"),
  releaseStepCommand("sonatypeBundleRelease"),
  setNextVersion,
  commitNextVersion,
  pushChanges
)

lazy val root = project.in(file(".")).aggregate(roll20CoreJS, roll20CoreJVM)


lazy val roll20Core = crossProject(JSPlatform, JVMPlatform).in(file(".")).
  settings(
    name := "Roll20 Core",
    libraryDependencies += "org.scalatest" %%% "scalatest" % "3.2.5" % "test",
    publish / skip := false
  ).
  jvmSettings(
    // Add JVM-specific settings here
    parallelExecution in Test := false,
    logBuffered in Test := false
  ).
  jsSettings(
    // Add JS-specific settings here
  )

lazy val roll20CoreJVM = roll20Core.jvm
lazy val roll20CoreJS = roll20Core.js
