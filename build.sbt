val dottyVersion = "3.0.0-RC2"

ThisBuild / scalaVersion := dottyVersion

ThisBuild / resolvers += Resolver.bintrayRepo("opencaesar", "oml")
ThisBuild / resolvers += Resolver.bintrayRepo("opencaesar", "oml-tools")

lazy val root = (project in file("."))
  .settings(
    name := "scala3-bundle-closure",
    version := "0.1.0",
    
    Compile / scalacOptions :=
      Seq(
          "-feature",
          "-unchecked",
          "-Xfatal-warnings",
          "-new-syntax",
          "-explain",
          "-explain-types",
          "-language:implicitConversions",
          "-Ykind-projector",
          "-Xignore-scala2-macros"
      ),

    ThisBuild / libraryDependencies ++= Seq(
      //"com.github.julien-truffaut" %% "monocle-core" % "3.0.0-M4",

      // not yet available for 3.0.0-RC2
      //"dev.zio" %% "zio" % "1.0.5",
      //"pl.iterators" % "kebs-tagged" % "1.9.0",

      "org.typelevel" %% "cats-effect"     % "3.0.1",
      "org.typelevel" %% "cats-core" % "2.5.0",
      "org.typelevel" %% "cats-free" % "2.5.0",
      //"io.github.vigoo" % "clipp-zio_2.13" % "0.6.0",
      "net.sourceforge.owlapi" % "owlapi-distribution" % "5.1.+",
      "io.opencaesar.oml" % "io.opencaesar.oml.dsl" % "0.8.+",
      "io.opencaesar.oml" % "oml-validate" % "0.8.+",
      "com.novocode" % "junit-interface" % "0.11" % "test"
    )
  )
