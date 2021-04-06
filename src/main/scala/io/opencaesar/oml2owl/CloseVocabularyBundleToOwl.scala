package io.opencaesar.oml2owl

import io.opencaesar.graph.*
import io.opencaesar.graph.Axiom.*
import io.opencaesar.graph.ClassExpression.*
import io.opencaesar.graph.DFS.*
import io.opencaesar.oml.{Concept, Ontology, RelationEntity, SpecializationAxiom}
import io.opencaesar.oml.util.OmlRead
import org.semanticweb.owlapi.model.{IRI, OWLAxiom, OWLClassExpression, OWLOntology}

import scala.jdk.CollectionConverters.*
import java.util

case class CloseVocabularyBundleToOwl
( o: Ontology,
  owlOntology: OWLOntology,
  disjointUnions: Boolean,
  owlApi: OwlApi ):

  def run(): Unit =
    val allOntologies = OmlRead.reflexiveClosure(o, OmlRead.getImportedOntologies).asScala.toSet
    val t1 = allOntologies.foldLeft(DiGraph.empty[ClassExpression[String]]) { case (ti, o) =>
      o.eAllContents().asScala.foldLeft(ti) {
        case (tj, c: Concept) =>
          tj.addVertex(ClassExpression.Singleton(OmlRead.getIri(c)))
        case (tj, re: RelationEntity) =>
          tj.addVertex(ClassExpression.Singleton(OmlRead.getIri(re)))
        case (tj, _) =>
          tj
      }
    }
    val t2 = allOntologies.foldLeft(t1) { case (ti, o) =>
      o.eAllContents().asScala.foldLeft(ti) {
        case (tj, ax: SpecializationAxiom) =>
          val specializedSingleton = ClassExpression.Singleton(OmlRead.getIri(ax.getSpecializedTerm))
          val specializingSingleton = ClassExpression.Singleton(OmlRead.getIri(OmlRead.getSpecificTerm(ax)))
          tj.addEdge(specializedSingleton, specializingSingleton)
        case (tj, _) =>
          tj
      }
    }
    val conceptTaxonomy = t2.transitiveReduction().rootAt(ClassExpression.Universal())
    val axiomType: Axiom = if disjointUnions then
      Axiom.DISJOINT_UNION
    else
      Axiom.DISJOINT_CLASSES
    conceptTaxonomy.generateClosureAxioms(axiomType).foreach { ax =>
      owlApi.addAxiom(owlOntology, toOwlAxiom(ax)(using owlApi))
    }
    ()

  def toOwlAxiom(ax: ClassExpressionSetAxiom[String])(using owlApi: OwlApi): OWLAxiom =
    val oces: util.stream.Stream[OWLClassExpression] = ax.set.map(toOwlClassExpression).asJava.stream
    ax match
      case _: DisjointClassesAxiom[String] =>
        owlApi.getOWLDisjointClassesAxiom(oces)
      case _: EquivalentClassesAxiom[String] =>
        owlApi.getOWLEquivalentClassesAxiom(oces)
      case DisjointUnionAxiom(Singleton(c),cs) =>
        owlApi.getOWLDisjointUnionAxiom(owlApi.getOWLClass(IRI.create(c)), oces)

  def toOwlClassExpression(ce: ClassExpression[String])(using owlApi: OwlApi): OWLClassExpression =
    ce match
      case Universal() =>
        owlApi.getOWLThing
      case Empty() =>
        owlApi.getOWLNothing
      case Singleton(c) =>
        owlApi.getOWLClass(IRI.create(c))
      case Complement(e) =>
        owlApi.getOWLObjectComplementOf(toOwlClassExpression(e))
      case Difference(a,b) =>
        toOwlClassExpression(a.intersection(b.complement()))
      case Intersection(s) =>
        owlApi.getOWLObjectIntersectionOf(s.map(toOwlClassExpression).asJava.stream)
      case Union(s) =>
        owlApi.getOWLObjectUnionOf(s.map(toOwlClassExpression).asJava.stream)
