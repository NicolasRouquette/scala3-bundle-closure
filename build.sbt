val dottyVersion = "3.0.0-RC1"

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
    name := "scala3-bundle-closure",
    version := "0.1.0",

    scalaVersion := dottyVersion,

    libraryDependencies += "com.novocode" % "junit-interface" % "0.11" % "test"
  )
