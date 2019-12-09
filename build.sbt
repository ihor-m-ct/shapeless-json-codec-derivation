name := "scala-shapeless-json-derivation"

version := "0.1"

scalaVersion := "2.13.1"

addCompilerPlugin("io.tryp" % "splain" % "0.5.0" cross CrossVersion.patch)

libraryDependencies ++= Seq(
  "com.chuusai" %% "shapeless" % "2.3.3"
)
