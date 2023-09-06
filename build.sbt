ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.3.0"

ThisBuild / organization := "com.peknight"

lazy val commonSettings = Seq(
  scalacOptions ++= Seq(
    "-feature",
    "-deprecation",
    "-unchecked",
    "-Xfatal-warnings",
    "-language:strictEquality",
    "-Xmax-inlines:64"
  ),
)

lazy val password = (project in file("."))
  .aggregate(
    passwordCore.jvm,
    passwordCore.js,
  )
  .settings(commonSettings)
  .settings(
    name := "password",
  )

lazy val passwordCore = (crossProject(JSPlatform, JVMPlatform) in file("password-core"))
  .settings(commonSettings)
  .settings(
    name := "password-core",
    libraryDependencies ++= Seq(
      "com.peknight" %%% "random-core" % pekRandomVersion,
      "com.peknight" %%% "validation-spire" % pekValidationVersion,
      "com.peknight" %%% "spire-ext" % pekSpireExtVersion,
    ),
  )

val pekRandomVersion = "0.1.0-SNAPSHOT"
val pekValidationVersion = "0.1.0-SNAPSHOT"
val pekSpireExtVersion = "0.1.0-SNAPSHOT"
