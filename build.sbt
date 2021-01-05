val dottyVersion = "3.0.0-M2"

scalacOptions := 
  Seq(
    "-feature",
    "-unchecked",
    "-language:implicitConversions",
    "-Xfatal-warnings",
    "-new-syntax"
  )

lazy val root = project
  .in(file("."))
  .settings(
    name := "dotty-simple",
    version := "0.1.0",

    scalaVersion := dottyVersion,

    libraryDependencies += "com.novocode" % "junit-interface" % "0.11" % "test"
  )
