name := """datasize-calculator"""

lazy val root = (project in file(".")).enablePlugins(PlayScala)

scalaVersion := "2.12.8"

libraryDependencies += guice

herokuAppName in Compile := "datasize-calculator"