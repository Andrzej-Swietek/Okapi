ThisBuild / organization := "io.github.andrzej-swietek"
ThisBuild / version := "0.1.3"
ThisBuild / scalaVersion := "3.6.4"

lazy val core = project.in(file("core"))

lazy val root = (project in file("."))
  .aggregate(core)
  .settings(
    publish / skip := true,
  )
