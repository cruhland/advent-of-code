
ThisBuild / scalaVersion := "2.13.15"

ThisBuild / libraryDependencies ++= Seq(
  "org.typelevel" %% "cats-core" % "2.12.0",
  "org.scalatest" %% "scalatest" % "3.2.19" % "test",
)

ThisBuild / scalacOptions ++= Seq("-deprecation")
