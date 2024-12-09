ThisBuild / scalaVersion := "3.6.2"
ThisBuild / libraryDependencies ++= Seq(
  "org.scala-lang.modules" %% "scala-parser-combinators" % "2.4.0",
  "org.typelevel" %% "cats-core" % "2.12.0",
  "org.typelevel" %% "kittens" % "3.4.0",
  "org.scalatest" %% "scalatest" % "3.2.19" % Test
)

ThisBuild / scalacOptions ++= Seq(
  "-deprecation",
  "-encoding",
  "UTF-8",
  "-feature",
  "-unchecked",
  "-Xkind-projector:underscores"
)

lazy val core =
  project.in(file("core")).settings(name := "scala3-lambda-calculus-core")

lazy val cliFrontend =
  project
    .in(file("frontends") / "cli")
    .dependsOn(core)
    .settings(name := "scala3-lambda-calculus-cli")
