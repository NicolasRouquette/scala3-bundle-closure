package io.opencaesar.oml2owl

import io.opencaesar.oml.dsl.OmlStandaloneSetup
import io.opencaesar.oml.util.OmlXMIResourceFactory

import io.github.vigoo.clipp.*
import io.github.vigoo.clipp.parsers.*
import io.github.vigoo.clipp.syntax.*
import io.github.vigoo.clipp.zioapi.*
import zio.*
import zio.console.Console

import java.nio.charset.Charset

object Oml2OwlApp extends zio.App:

  override def run(args: List[String]): URIO[zio.ZEnv, ExitCode] =

    val parameters: ZIO[Console, ParserFailure, Parameters] = Clipp.parseOrFail[Parameters](args, Parameters.parser)
    val program = for {
      p <- parameters
      result <- run(p)
    } yield result

    program
      .catchAll {
        case (_: ParserFailure) =>
          ZIO.succeed(ExitCode.failure)
        case (_: Throwable) =>
          ZIO.succeed(ExitCode.failure)
      }

  def run(p: Parameters): Task[ExitCode] =
    for {
      _ <- ZIO.effect(println(s"Input catalog path= ${p.inputCatalogPath}"))
      _ <- ZIO.effect(println(s"Output catalog path= ${p.outputCatalogPath}"))
      _ = OmlStandaloneSetup.doSetup()
      _ = OmlXMIResourceFactory.register()
    } yield ExitCode.success


