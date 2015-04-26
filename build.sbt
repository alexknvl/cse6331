assemblyJarName in assembly := "cse6331.jar"

name := "cse6331"

version := "0.1-SNAPSHOT"

organization := "com.alexknvl"

scalaVersion := "2.11.6"

scalacOptions ++= Seq(
  "-deprecation",
  "-feature",
  "-optimise",
  "-Yclosure-elim",
  "-Yinline",
  "-Xplugin-require:scalaxy-streams"
)

resolvers ++= Seq(
  Resolver.sonatypeRepo("releases"),
  Resolver.sonatypeRepo("snapshots")
)

autoCompilerPlugins := true

addCompilerPlugin("com.nativelibs4java" %% "scalaxy-streams" % "0.3.2")

libraryDependencies ++= Seq(
  "org.spire-math" %% "spire" % "0.9.0",
  "com.chuusai" %% "shapeless" % "2.0.0",
  "org.scalanlp" %% "breeze" % "0.10",
  "org.scalanlp" %% "breeze-natives" % "0.10"
)

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "2.2.2" % "test",
  "com.storm-enroute" %% "scalameter" % "0.7-SNAPSHOT" % "test",
  "org.scalacheck" %% "scalacheck" % "1.12.1" % "test"
)

testFrameworks += new TestFramework("org.scalameter.ScalaMeterFramework")

parallelExecution in Test := false
