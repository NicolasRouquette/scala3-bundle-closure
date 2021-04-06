package io.opencaesar.oml2owl

import org.eclipse.emf.common.util.URI
import org.eclipse.emf.ecore.util.ECrossReferenceAdapter
import org.eclipse.xtext.resource.XtextResourceSet
import io.opencaesar.oml.{Ontology, DescriptionBundle, VocabularyBundle}
import io.opencaesar.oml.dsl.OmlStandaloneSetup
import io.opencaesar.oml.util.OmlCatalog
import io.opencaesar.oml.util.OmlRead
import io.opencaesar.oml.util.OmlXMIResourceFactory
import io.opencaesar.oml.validate.OmlValidator
import io.github.vigoo.clipp.*
import io.github.vigoo.clipp.parsers.*
import io.github.vigoo.clipp.syntax.*
import io.github.vigoo.clipp.catseffect3.*
import cats.effect.*

import org.semanticweb.owlapi.model.{IRI,OWLOntology}
import org.semanticweb.owlapi.apibinding.OWLManager
import java.io.File
import java.nio.charset.Charset
import javax.swing.text.html.parser.Parser
import scala.util.{Failure, Success, Try}
import scala.jdk.CollectionConverters.*

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
      inputResourceSet = new XtextResourceSet()
      _ = inputResourceSet.eAdapters().add(new ECrossReferenceAdapter())
      inputCatalog = OmlCatalog.create(p.inputCatalogPath.toURI().toURL())
      rootUri <- resolveRootOntologyIri(p.rootOntologyIri, inputCatalog)
      rootOntology = OmlRead.getOntology(inputResourceSet.getResource(rootUri, true))
      inputOntologies = OmlRead.getAllImportedOntologiesInclusive(rootOntology)

      // validate ontologies
      _ <- validateOntologies(inputOntologies.asScala)

      // create OWL manager
      ontologyManager = OWLManager.createOWLOntologyManager()
      owl2api = OwlApi(ontologyManager, p.annotationsOnAxioms)

      // create the equivalent OWL ontologies
      tuples = createOutputOWLOntologies(owl2api, inputResourceSet, p.inputCatalogPath.getParentFile, p.outputCatalogPath.getParent)

      // run the vocabulary bundle closure algorithm
      _ = tuples.foreach {
        case (b: VocabularyBundle, _, owlOntology) =>
          //CloseVocabularyBundleToOwl(b, owlOntology, p.disjointUnions, owl2api).run
        case _ =>
          ()
      }

      // run the description bundle closure algorithm
      _ = tuples.foreach {
        case (d: DescriptionBundle, _, owlOntology) =>
          //CloseDescriptionBundleToOwl(d, owlOntology, p.disjointUnions, owl2api).run
        case _ =>
          ()
      }

      // save the output resources
      _ <- tuples.foldLeft(IO.pure(ExitCode.Success)) { case (acc, (o, outputFile, owlOntology)) =>
        acc >> (Try(ontologyManager.saveOntology(owlOntology, IRI.create(outputFile))) match
          case Success(_) =>
            IO.pure(ExitCode.Success)
          case Failure(t) =>
            IO.raiseError(t)
          )
      }

      // create the equivalent OWL catalog
    } yield ExitCode.Success


  val OML = ".oml"
  val OMLXMI = ".omlxmi"

  def resolveRootOntologyIri(iri: String, catalog: OmlCatalog): IO[URI] =
    val resolved = URI.createURI(catalog.resolveURI(iri))
    if resolved.isFile then
      val filename = resolved.toFileString
      if new File(filename).isFile then
        IO.pure(resolved)
      else if new File(filename + OML).isFile then
        IO.pure(URI.createFileURI(filename+OML))
      else if new File(filename + OMLXMI).isFile then
        IO.pure(URI.createFileURI(filename+OMLXMI))
      else
        IO.raiseError(new IllegalArgumentException(s"root ontology must resolve to an OML file; got: $iri"))
    else
      IO.raiseError(new IllegalArgumentException(s"root ontology must resolve to an OML file; got: $iri"))

  def validateOntologies(os: Iterable[Ontology]): IO[ExitCode] =
    os.foldLeft(IO.pure(ExitCode.Success)) { case (acc: IO[ExitCode], o: Ontology) =>
      acc >> (Try(OmlValidator.validate(o)) match
        case Success(_) =>
          IO.pure(ExitCode.Success)
        case Failure(t) =>
          IO.raiseError(t)
        )
    }

  def createOutputOWLOntologies
  (owl2api: OwlApi, rs: XtextResourceSet, inputFolder: File, outputFolderPath: String): Set[(Ontology, File, OWLOntology)] =
    rs.getResources.asScala.foldLeft(Set.empty[(Ontology, File, OWLOntology)]) { case (acc, r) =>
      val o = OmlRead.getOntology(r)
      if null != o && !Oml2Owl.isBuiltInOntology(o.getIri) then
        val uri = new java.net.URI(r.getURI.toString)
        val relativePath = outputFolderPath + File.separator + inputFolder.toURI().relativize(uri).getPath()
        val outputFile = new File(relativePath.substring(0, relativePath.lastIndexOf('.')+1)+"owl")
        val owlOntology = new Oml2Owl(r, owl2api).run
        acc + Tuple3(OmlRead.getOntology(r), outputFile, owlOntology)
      else
        acc
    }