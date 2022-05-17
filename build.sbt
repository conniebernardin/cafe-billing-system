ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.8"

lazy val root = (project in file("."))
  .settings(
    name := "CafeSystem"
  )

libraryDependencies ++= Seq(
  "org.scalatest"          %% "scalatest"               % "3.2.5"             % Test,
  "org.scalatestplus.play" %% "scalatestplus-play"   % "5.0.0"          % Test
)