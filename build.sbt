name := "brainsuck"

version := "1.0"

scalaVersion := "2.11.12"

enablePlugins(JavaAppPackaging)

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "3.0.5",
  "com.github.scopt" %% "scopt" % "3.7.0",
  "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.1"
)
