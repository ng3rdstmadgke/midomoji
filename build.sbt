import Dependencies._

lazy val root = (project in file(".")).
  settings(
    organization := "com.github.ng3rdstmadgke.midomoji",
    scalaVersion := "2.12.5",
    version      := "0.1.0-SNAPSHOT",
    mainClass in assembly := Some("com.github.ng3rdstmadgke.midomoji"),
    assemblyJarName in assembly := "midomoji.jar",
    name := "midomoji",
    libraryDependencies += scalaTest % Test
  )
