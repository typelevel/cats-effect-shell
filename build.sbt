ThisBuild / tlBaseVersion := "1.0"

ThisBuild / scalaVersion := "3.3.1"

ThisBuild / organization := "org.typelevel"
ThisBuild / organizationName := "Typelevel"
ThisBuild / tlSonatypeUseLegacyHost := false
ThisBuild / tlJdkRelease := None

ThisBuild / startYear := Some(2023)

ThisBuild / developers := List(
  Developer(
    "mpilquist",
    "Michael Pilquist",
    "mpilquist@gmail.com",
    url("https://github.com/mpilquist")
  )
)

ThisBuild / homepage := Some(url("https://github.com/typelevel/cats-effect-shell"))

ThisBuild / scmInfo := Some(
  ScmInfo(
    url("https://github.com/typelevel/cats-effect-shell"),
    "git@github.com:typelevel/cats-effect-shell.git"
  )
)

ThisBuild / githubWorkflowJavaVersions := Seq(JavaSpec.temurin("17"))
ThisBuild / scalacOptions += "-source:future"

lazy val root = project
  .in(file("."))
  .settings(
    name := "cats-effect-shell",
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-effect" % "3.5.2",
      "co.fs2" %% "fs2-core" % "3.9.2",
      "com.olvind.tui" %% "tui" % "0.0.7"
    )
  )
