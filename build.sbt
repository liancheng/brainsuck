name := "brainsuck"
version := "1.0"

scalaVersion := "2.13.8"
scalacOptions ++= Seq(
  "-deprecation",
  "-Ywarn-unused"
)

semanticdbEnabled := true
semanticdbVersion := scalafixSemanticdb.revision

libraryDependencies ++= Seq(
  "com.github.scopt" %% "scopt" % "4.0.0-RC2",
  "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.2",
  "org.scalatest" %% "scalatest" % "3.2.0" % "test"
)

ThisBuild / scalafixDependencies += "com.github.liancheng" %% "organize-imports" % "0.6.0"

enablePlugins(JavaAppPackaging)
