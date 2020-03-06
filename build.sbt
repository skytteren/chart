
import sbtcrossproject.CrossPlugin.autoImport.{crossProject, CrossType}

enablePlugins(GitVersioning)
enablePlugins(GitBranchPrompt)

scalaVersion := "2.13.1"

lazy val commonSettings: Seq[Def.Setting[_]] =
  Seq(
    organization         := "no.skytteren",
    scalaVersion         := "2.13.1",
    scalacOptions       ++= Seq("-encoding", "UTF-8", "-feature", "-unchecked", "-Xlint", "-deprecation"),
    fork in run          := true,
    cancelable in Global := true,
    resolvers            += "Local Maven Repository" at "file://"+Path.userHome.absolutePath+"/.m2/repository"
  )

lazy val chart = crossProject(JSPlatform, JVMPlatform)
  .in(file("."))
  .settings(commonSettings :_*)
  .settings(
    libraryDependencies += "com.lihaoyi" %%% "scalatags" % "0.8.6",
    libraryDependencies += "org.scalatest" %%% "scalatest" % "3.1.1" % "test",
    libraryDependencies += "no.skytteren" %%% "scalatime" % "0900ddf1c8ebbe6a491da3e016245703db32faf6",
  )

lazy val chartJvm = chart.jvm
lazy val chartJs = chart.js

lazy val server = project
  .dependsOn(chartJvm)
  .settings(commonSettings :_*)
  .settings(
    libraryDependencies += "com.typesafe.akka" %% "akka-http" % "10.1.11",
    libraryDependencies += "com.typesafe.akka" %% "akka-stream" % "2.6.3",
  )

lazy val root = project
  .in(file("."))
  .settings(commonSettings :_*)
  .aggregate(chartJvm, chartJs, server)
  .enablePlugins(ScalaJSPlugin)
  .settings(
    name := "chart_root",
    publish := {},
    publishLocal := {}
  )
