name := "annotator"

version := "0.1"
organization := "dev.nigredo"
scalaVersion := "2.11.11"

libraryDependencies += "com.github.scopt" %% "scopt" % "3.6.0"
libraryDependencies += "org.scalameta" %% "scalameta" % "1.8.0"

addCompilerPlugin("org.scalameta" % "paradise" % "3.0.0-M9" cross CrossVersion.full)

crossPaths := false

assemblyJarName in ThisBuild := s"${name.value}-${version.value}.jar"