enablePlugins(ScalaJSPlugin)

name := "Roll20 Core Root"

organization in ThisBuild := "com.lkroll.roll20"

version in ThisBuild := "0.11.0-SNAPSHOT"

scalaVersion in ThisBuild := "2.12.4"
crossScalaVersions in ThisBuild := Seq("2.11.11", "2.12.4")

resolvers += "Apache" at "http://repo.maven.apache.org/maven2"
resolvers += Resolver.bintrayRepo("lkrollcom", "maven")
resolvers += Resolver.mavenLocal

lazy val root = project.in(file(".")).
  aggregate(roll20CoreJS, roll20CoreJVM). //, sheetframeworkPlugin).
  settings(
    publish := {},
    publishLocal := {}
  )

lazy val roll20Core = crossProject.in(file(".")).
  settings(
    name := "Roll20 Core",
    libraryDependencies += "org.scalatest" %%% "scalatest" % "3.0.4" % "test",
    EclipseKeys.useProjectId := true,
    EclipseKeys.eclipseOutput := Some("./etarget")
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