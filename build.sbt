
import sbtcrossproject.CrossPlugin.autoImport.{crossProject, CrossType}

enablePlugins(GitVersioning)
enablePlugins(GitBranchPrompt)

This / scalaVersion := "3.1.2"

lazy val commonSettings: Seq[Def.Setting[_]] =
  Seq(
    organization := "no.skytteren",
    scalaVersion := "3.1.2",
    scalacOptions ++= Seq(
      "-encoding", "UTF-8",
      "-feature",
      "-unchecked",
      "-source:3.0-migration",
      "-rewrite",
      "-deprecation",
      "-indent",

    ),
    run / fork := true,
    Global / cancelable  := true,
    resolvers += "Local Maven Repository" at "file://" + Path.userHome.absolutePath + "/.m2/repository"
  )

lazy val chart = crossProject(JSPlatform, JVMPlatform)
  .in(file("."))
  .settings(commonSettings: _*)
  .settings(
    libraryDependencies ++= Seq(
      "com.lihaoyi" %%% "scalatags" % "0.11.1",
      "org.scalatest" %%% "scalatest" % "3.2.12" % "test",
      "no.skytteren" %%% "scalatime" % "72b53fb0a9b2e6924d88d658716972e90e760a57",
    )
  )

lazy val chartJvm = chart.jvm
lazy val chartJs = chart.js

/*
lazy val server = project
  .dependsOn(chartJvm)
  .settings(commonSettings: _*)
  .settings(
    name := "chartServer",
    libraryDependencies += "com.typesafe.akka" %% "akka-http" % "10.2.9",
    libraryDependencies += "com.typesafe.akka" %% "akka-stream" % "2.6.19",
  )

 */

lazy val root = project
  .in(file("."))
  .settings(commonSettings: _*)
  .aggregate(chartJvm, chartJs /*, server*/)
  .enablePlugins(ScalaJSPlugin)
  .settings(
    name := "chart_root",
    publish := {},
    publishLocal := {}
  )
