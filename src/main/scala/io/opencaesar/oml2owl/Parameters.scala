package io.opencaesar.oml2owl
import cats.free.Free

import java.io.File
import pl.iterators.kebs.tagged.*
import io.github.vigoo.clipp.{Parameter, ParserFailure, ParameterParser, errors}
import io.github.vigoo.clipp.parsers.*
import io.github.vigoo.clipp.syntax.*
import io.opencaesar.oml2owl.taggedTypes.*
import scala.util.Try

case class Parameters(
    inputCatalogPath: File @@ ReadableCatalog,
    rootOntologyIri: String,
    outputCatalogPath: File,
    disjointUnions: Boolean = false,
    annotationsOnAxioms: Boolean = false,
    debug: Boolean = false)

object Parameters:


  implicit val booleanParameterParser: ParameterParser[Boolean] = new ParameterParser[Boolean] {
    override def parse(value: String): Either[String, Boolean] = Try(value.toBoolean).toEither.left.map(_.getMessage)

    override def example: Boolean = false
  }

  val parser: Free[Parameter, Parameters] = for {
    _ <- metadata(
      programName = "oml2owl"
    )
    inputCatalogPath <- namedParameter[File @@ ReadableCatalog](
      description = "Path of the input catalog.xml file",
      placeholder = "input-catalog",
      shortName = 'i',
      longNames = "input-catalog-path"
    )
    rootOntologyIri <- namedParameter[String](
      description = "Root ontology IRI",
      placeholder = "root-iri",
      shortName = 'r',
      longNames = "root-ontology-iri"
    )
    outputCatalogPath <- namedParameter[File @@ WriteableCatalog](
      description = "Path of the output catalog.xml file",
      placeholder = "output-catalog",
      shortName = 'o',
      longNames = "output-catalog-path"
    )
    disjointUnions <- namedParameter[Boolean](
      description = "Create disjoint union axioms",
      placeholder = "disjoint-unions",
      shortName = 'u',
      longNames = "disjoint-unions"
    )
    annotationsOnAxioms <- namedParameter[Boolean](
      description = "Emit annotations on axioms",
      placeholder = "axiom-annotations",
      shortName = 'a',
      longNames = "annotations-on-axioms"
    )
    debug <- namedParameter[Boolean](
      description = "Shows debug logging statements",
      placeholder = "debug",
      shortName = 'd',
      longNames = "debug"
    )
  } yield Parameters(inputCatalogPath, rootOntologyIri, outputCatalogPath, disjointUnions, annotationsOnAxioms, debug)