package io.opencaesar.oml2owl

import io.opencaesar.oml.dsl.OmlStandaloneSetup
import io.opencaesar.oml.util.OmlXMIResourceFactory
import io.github.vigoo.clipp.*
import io.github.vigoo.clipp.parsers.*
import io.github.vigoo.clipp.syntax.*
import io.github.vigoo.clipp.catseffect3.*
import cats.effect.*

import java.nio.charset.Charset
import javax.swing.text.html.parser.Parser

object Oml2OwlApp extends IOApp:

  override def run(args: List[String]): IO[ExitCode] = {
    for {
      parameters <- Clipp.parseOrFail(args, Parameters.parser)
      status <- run(parameters)
    } yield status
  }

  def run(p: Parameters): IO[ExitCode] =
    for {
      _ <- IO.println(s"Input catalog path = ${p.inputCatalogPath}")
      _ <- IO.println(s"Root ontology IRI  = ${p.rootOntologyIri}")
      _ <- IO.println(s"Output catalog path= ${p.outputCatalogPath}")
      _ = OmlStandaloneSetup.doSetup()
      _ = OmlXMIResourceFactory.register()
    } yield ExitCode.Success


