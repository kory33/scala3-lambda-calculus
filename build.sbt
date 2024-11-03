scalaVersion := "3.5.2"
libraryDependencies ++= Seq(
  "org.scala-lang.modules" %% "scala-parser-combinators" % "2.4.0",
  "org.typelevel" %% "cats-core" % "2.12.0",
  "org.typelevel" %% "kittens" % "3.4.0",
  "org.scalatest" %% "scalatest" % "3.2.19" % Test
)

scalacOptions ++= Seq(
  "-deprecation",
  "-encoding",
  "UTF-8",
  "-feature",
  "-unchecked",
  "-Ypartial-unification",
  "-Ykind-projector:underscores"
)
