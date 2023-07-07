ThisBuild / tlBaseVersion := "1.0"

ThisBuild / scalaVersion := "3.3.0"

ThisBuild / organization := "org.typelevel"
ThisBuild / organizationName := "Typelevel"

ThisBuild / startYear := Some(2023)

ThisBuild / developers := List(
  Developer(
    "mpilquist",
    "Michael Pilquist",
    "mpilquist@gmail.com",
    url("https://github.com/mpilquist")),
)

ThisBuild / homepage := Some(url("https://github.com/typelevel/cats-effect-shell"))

ThisBuild / scmInfo := Some(
  ScmInfo(
    url("https://github.com/typelevel/cats-effect-shell"),
    "git@github.com:typelevel/cats-effect-shell.git"))


lazy val root = project
  .in(file("."))
  .settings(
    name := "cats-effect-shell",
  )

