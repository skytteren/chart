
import sbtcrossproject.CrossPlugin.autoImport.{crossProject, CrossType}

enablePlugins(GitVersioning)
enablePlugins(GitBranchPrompt)

scalaVersion := "2.13.0"

lazy val commonSettings: Seq[Def.Setting[_]] =
  Seq(
    organization         := "no.skytteren",
    scalaVersion         := "2.13.0",
    scalacOptions       ++= Seq("-encoding", "UTF-8", "-feature", "-unchecked", "-Xlint", "-deprecation"),
    fork in run          := true,
    cancelable in Global := true,
    resolvers            += "Local Maven Repository" at "file://"+Path.userHome.absolutePath+"/.m2/repository"
  )

lazy val chart = crossProject(JSPlatform, JVMPlatform)
  .in(file("."))
  .settings(commonSettings :_*)
  .settings(
    libraryDependencies += "com.lihaoyi" %%% "scalatags" % "0.7.0",
    libraryDependencies += "org.scalatest" %%% "scalatest" % "3.0.8" % "test",
    libraryDependencies += "no.skytteren" %%% "scalatime" % "e171ee56724813d23ff124a30dc26f4bd950cec5",
  )

lazy val chartJvm = chart.jvm
lazy val chartJs = chart.js

lazy val server = project
  .dependsOn(chartJvm)
  .settings(commonSettings :_*)
  .settings(
    libraryDependencies += "com.typesafe.akka" %% "akka-http" % "10.1.9",
    libraryDependencies += "com.typesafe.akka" %% "akka-stream" % "2.5.24",
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