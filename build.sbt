import sbt.Path

enablePlugins(ScalaJSPlugin)

version := "0.1"

scalaVersion := "2.12.4"


lazy val commonSettings: Seq[Def.Setting[_]] =
  Seq(
    organization         := "no.skytteren",
    scalaVersion         := "2.12.4",
    scalacOptions       ++= Seq("-encoding", "UTF-8", "-feature", "-unchecked", "-Xlint", "-Yno-adapted-args", "-Xfuture", "-deprecation"),
    fork in run          := true,
    cancelable in Global := true,
    resolvers            += "Local Maven Repository" at "file://"+Path.userHome.absolutePath+"/.m2/repository"
  )

lazy val chart = crossProject
  .in(file("."))
  .settings(commonSettings :_*)
  .settings(
    libraryDependencies += "com.lihaoyi" %%% "scalatags" % "0.6.5",
    libraryDependencies += "com.typesafe.akka" %% "akka-http" % "10.0.13",
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.5" % "test"
  )

lazy val chartJvm = chart.jvm
lazy val chartJs = chart.js

lazy val root = project
  .in(file("."))
  .settings(commonSettings :_*)
  .aggregate(chartJvm, chartJs)
  .enablePlugins(ScalaJSPlugin)
  .settings(
    name := "chart_root",
    publish := {},
    publishLocal := {}
  )