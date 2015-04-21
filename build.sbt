name := "brainsuck"

version := "1.0"

scalaVersion := "2.10.4"

enablePlugins(JavaAppPackaging)

libraryDependencies ++= Seq(
  "org.scalatest"    %% "scalatest" % "2.2.3",
  "com.github.scopt" %% "scopt"     % "3.3.0"
)
