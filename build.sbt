
import sbtcrossproject.CrossPlugin.autoImport.{crossProject, CrossType}

enablePlugins(GitVersioning)
enablePlugins(GitBranchPrompt)

scalaVersion := "2.13.3"

lazy val commonSettings: Seq[Def.Setting[_]] =
  Seq(
    organization         := "no.skytteren",
    scalaVersion         := "2.13.3",
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
    libraryDependencies += "no.skytteren" %%% "scalatime" % "07458a6bfc13726416e0912b87c37067a5ce3db5",
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
