import Dependencies._

lazy val root = (project in file(".")).
  settings(
    organization := "com.github.ng3rdstmadgke.midomoji",
    name := "midomoji",
    scalaVersion := "2.12.5",
    version      := "0.1.0-SNAPSHOT",
    //test in assembly := {},
    mainClass in assembly := Some("com.github.ng3rdstmadgke.midomoji.Main"),
    assemblyJarName in assembly := "midomoji.jar",
    libraryDependencies += scalaTest % Test,
    // https://mvnrepository.com/artifact/com.esotericsoftware/kryo
    libraryDependencies += "com.esotericsoftware" % "kryo" % "4.0.2"

  )
