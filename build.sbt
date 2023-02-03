// format: off
val projectName = "dfhdl"
val compilerVersion = "3.3.0-RC2"

inThisBuild(
  List(
    homepage := Some(url("https://dfianthdl.github.io/")),
    licenses := List(
      "LGPL" -> url("https://www.gnu.org/licenses/lgpl-3.0.txt")
    ),
    developers := List(
      Developer(
        "soronpo",
        "Oron Port",
        "",
        url("https://www.researchgate.net/profile/Oron_Port")
      )
    )
  )
)

name := projectName
ThisBuild / organization := "io.github.dfianthdl"
ThisBuild / scalaVersion := compilerVersion
ThisBuild / version      := "0.2.3-SNAPSHOT"


// PROJECTS
lazy val root = (project in file("."))
  .settings(
    settings,
    publish / skip := true
  )
  .aggregate(
    core
  )



lazy val core = project
  .settings(
    name := s"$projectName-core",
    settings,
    libraryDependencies ++= commonDependencies :+ dependencies.scalafmt
  )


// DEPENDENCIES

lazy val dependencies =
  new {
    private val scodecV = "1.1.34"
    private val munitV = "0.7.29"
    private val scalafmtV = "3.3.1"
    private val airframelogV = "22.7.3"
    val scodec = "org.scodec" %% "scodec-bits" % scodecV
    val munit = "org.scalameta" %% "munit" % munitV % Test
    val scalafmt = ("org.scalameta" %% "scalafmt-dynamic" % scalafmtV).cross(CrossVersion.for3Use2_13)
    val airframelog = "org.wvlet.airframe" %% "airframe-log" % airframelogV
  }

lazy val commonDependencies = Seq(
  dependencies.scodec,
  dependencies.munit,
  dependencies.airframelog
)

// SETTINGS

lazy val settings =
  commonSettings

lazy val compilerOptions = Seq(
  "-unchecked",
  "-feature",
  "-language:strictEquality",
  "-language:implicitConversions",
  "-language:experimental",
  "-deprecation",
)

lazy val commonSettings = Seq(
  scalacOptions ++= compilerOptions
)
