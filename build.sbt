name := "brainsuck"

version := "1.0"

scalaVersion := "2.10.7"

enablePlugins(JavaAppPackaging)

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "3.0.5",
  "com.github.scopt" %% "scopt" % "3.7.0"
)
