val dottyVersion = "3.1.2"

ThisBuild / scalaVersion := dottyVersion

ThisBuild / resolvers += Resolver.bintrayRepo("opencaesar", "oml")

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
      //"com.github.julien-truffaut" %% "monocle-core" % "3.0.0-M4",
      "pl.iterators" % "kebs-tagged_2.13" % "1.9.0",
      "io.github.vigoo" %% "clipp-zio" % "0.6.7",
      "net.sourceforge.owlapi" % "owlapi-distribution" % "5.1.+",
      "io.opencaesar.oml" % "io.opencaesar.oml.dsl" % "0.8.+",
      "com.novocode" % "junit-interface" % "0.11" % "test"
    )
  )
