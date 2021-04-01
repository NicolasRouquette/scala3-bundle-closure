val dottyVersion = "3.0.0-RC2"

ThisBuild / scalaVersion := dottyVersion

lazy val root = (project in file("."))
  .settings(
    name := "scala3-bundle-closure",
    version := "0.1.0",
    
    Compile / scalacOptions :=
      Seq(
          "-feature",
          "-unchecked",
          "-language:implicitConversions",
          "-Xfatal-warnings",
          "-new-syntax"
      ),

    ThisBuild / libraryDependencies ++= Seq(
      "com.github.julien-truffaut" %% "monocle-core" % "3.0.0-M4",
      "com.novocode" % "junit-interface" % "0.11" % "test"
    )
  )
