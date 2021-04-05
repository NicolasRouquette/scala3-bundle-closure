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
    disjointUnions <- flag(
      description = "Create disjoint union axioms",
      shortName = 'u',
      longNames = "disjoint-unions"
    )
    annotationsOnAxioms <- flag(
      description = "Emit annotations on axioms",
      shortName = 'a',
      longNames = "annotations-on-axioms"
    )
    debug <- flag(
      description = "Shows debug logging statements",
      shortName = 'd',
      longNames = "debug"
    )
  } yield Parameters(inputCatalogPath, rootOntologyIri, outputCatalogPath, disjointUnions, annotationsOnAxioms, debug)