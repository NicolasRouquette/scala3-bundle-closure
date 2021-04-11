package io.opencaesar.oml2owl

import io.opencaesar.oml.*
import io.opencaesar.oml.util.{OmlIndex, OmlRead, OmlSearch}
import org.semanticweb.owlapi.model.*
import org.semanticweb.owlapi.model.parameters.ChangeApplied
import io.opencaesar.graph.DiGraph
import io.opencaesar.graph.DFS.dfs
import io.opencaesar.graph.TransitiveClosure
import io.opencaesar.graph.TransitiveClosure.transitiveClosure
import io.opencaesar.oml.{Literal, ScalarProperty}

import scala.collection.immutable.*
import scala.jdk.CollectionConverters.*
import java.util

case class CloseDescriptionBundleToOwl
(o: Ontology,
 owlOntology: OWLOntology,
 disjointUnions: Boolean,
 owlApi: OwlApi):

  import CloseDescriptionBundleToOwl.*

  def run(): Unit =
    val allOntologies: Set[Ontology] = OmlRead.reflexiveClosure(o, OmlRead.getImportedOntologies).asScala.toSet

    val entitiesWithRestrictedScalarProperties: Map[Entity, Set[ScalarProperty]]
    = getEntitiesWithRestrictedScalarProperties(allOntologies)

    val entitiesWithRestrictedStructuredProperties: Map[Entity, Set[StructuredProperty]]
    = getEntitiesWithRestrictedStructuredProperties(allOntologies)

    val entitiesWithRestrictedRelations: Map[Entity, Set[Relation]]
    = getEntitiesWithRestrictedRelations(allOntologies)

    val allRestrictedEntities: Set[Entity] =
      entitiesWithRestrictedScalarProperties.keySet ++
      entitiesWithRestrictedStructuredProperties.keySet ++
      entitiesWithRestrictedRelations.keySet

    val termSpecializations = getTermSpecializations(allOntologies)
    val propertyTrees = getPropertyTrees(allOntologies)
    val relationTrees = getRelationTrees(allOntologies)

    val entityInstances: Map[Entity, Set[NamedInstance]]
    = getEntityInstances(allOntologies, allRestrictedEntities, termSpecializations)

    // Generate data cardinality restrictions for scalar properties

    val scalarPropertyCounts: Map[NamedInstance, Map[ScalarProperty, Integer]]
    = getScalarPropertyCounts(entitiesWithRestrictedScalarProperties, entityInstances, termSpecializations, propertyTrees)

    ScalarFeatureCounts.generateCardinalityRestrictions(owlOntology, owlApi, scalarPropertyCounts)

    // Generate object cardinality restrictions for structured properties and relations.

    val structuredPropertyCounts
    : Map[NamedInstance, Map[StructuredProperty, Integer]]
    = getStructuredPropertyCounts(entitiesWithRestrictedStructuredProperties, entityInstances, termSpecializations, propertyTrees)

    StructuredFeatureCounts.generateCardinalityRestrictions(owlOntology, owlApi, structuredPropertyCounts)

    val relationCounts
    : Map[NamedInstance, Map[Relation, Integer]]
    = getRelationCounts(entitiesWithRestrictedRelations, entityInstances, termSpecializations, relationTrees)

    RelationCounts.generateCardinalityRestrictions(owlOntology, owlApi, relationCounts)

    ()

object CloseDescriptionBundleToOwl:

  def getEntitiesWithRestrictedScalarProperties
  (allOntologies: Set[Ontology])
  : Map[Entity, Set[ScalarProperty]]
  = allOntologies.foldLeft(Map.empty[Entity, Set[ScalarProperty]]) {
    case (acc1, o: Vocabulary) =>
      OmlRead.getStatements(o).asScala.foldLeft(acc1) {
        case (acc2, e: Entity) =>
          OmlSearch.findPropertyRestrictions(e).asScala.foldLeft(acc2) {
            case (acc3, ax: ScalarPropertyCardinalityRestrictionAxiom) =>
              ax.getKind match
                case _ @ ( CardinalityRestrictionKind.MIN | CardinalityRestrictionKind.EXACTLY ) =>
                  acc3.updated(e, acc3.getOrElse(e, Set.empty[ScalarProperty]) + ax.getProperty)
                case _ =>
                  acc3
            case (acc3, ax: ScalarPropertyRangeRestrictionAxiom) =>
              ax.getKind match
                case RangeRestrictionKind.SOME =>
                  acc3.updated(e, acc3.getOrElse(e, Set.empty[ScalarProperty]) + ax.getProperty)
                case _ =>
                  acc3
            case (acc3, _) =>
              acc3
          }
        case (acc2, _) =>
          acc2
      }
    case (acc1, _) =>
      acc1
  }

  def getEntitiesWithRestrictedStructuredProperties
  (allOntologies: Set[Ontology])
  : Map[Entity, Set[StructuredProperty]]
  = allOntologies.foldLeft(Map.empty[Entity, Set[StructuredProperty]]) {
    case (acc1, o: Vocabulary) =>
      OmlRead.getStatements(o).asScala.foldLeft(acc1) {
        case (acc2, e: Entity) =>
          OmlSearch.findPropertyRestrictions(e).asScala.foldLeft(acc2) {
            case (acc3, ax: StructuredPropertyCardinalityRestrictionAxiom) =>
              ax.getKind match
                case _ @ ( CardinalityRestrictionKind.MIN | CardinalityRestrictionKind.EXACTLY ) =>
                  acc3.updated(e, acc3.getOrElse(e, Set.empty[StructuredProperty]) + ax.getProperty)
                case _ =>
                  acc3
            case (acc3, ax: StructuredPropertyRangeRestrictionAxiom) =>
              ax.getKind match
                case RangeRestrictionKind.SOME =>
                  acc3.updated(e, acc3.getOrElse(e, Set.empty[StructuredProperty]) + ax.getProperty)
                case _ =>
                  acc3
            case (acc3, _) =>
              acc3
          }
        case (acc2, _) =>
          acc2
      }
    case (acc1, _) =>
      acc1
  }

  def getRelationsWithSWRLRules
  (allOntologies: Set[Ontology])
  : Map[Relation, Set[Rule]]
  = allOntologies.foldLeft(Map.empty[Relation, Set[Rule]]) {
    case (acc1, o: Vocabulary) =>
      OmlRead.getStatements(o).asScala.foldLeft(acc1) {
        case (acc2, r: Rule) =>
          r.getConsequent.asScala.foldLeft(acc2) {
            case (acc3, p: EntityPredicate) =>
              p.getEntity match
                case re: RelationEntity =>
                  re.getForwardRelation match
                    case fr: ForwardRelation =>
                      acc3.updated(fr, acc3.getOrElse(fr, Set.empty[Rule]) + r)
                    case null =>
                      acc3
                  re.getReverseRelation match
                    case rr: ReverseRelation =>
                      acc3.updated(rr, acc3.getOrElse(rr, Set.empty[Rule]) + r)
                    case null =>
                      acc3
                case _ =>
                  acc3

            case (acc3, p: RelationPredicate) =>
              p.getRelation match
                case rel: Relation =>
                  acc3.updated(rel, acc3.getOrElse(rel, Set.empty[Rule]) + r)
                case null =>
                  acc3

            case (acc3, p: RelationEntityPredicate) =>
              p.getEntity match
                case re: RelationEntity =>
                  re.getForwardRelation match
                    case fr: ForwardRelation =>
                      acc3.updated(fr, acc3.getOrElse(fr, Set.empty[Rule]) + r)
                    case null =>
                      acc3
                  re.getReverseRelation match
                    case rr: ReverseRelation =>
                      acc3.updated(rr, acc3.getOrElse(rr, Set.empty[Rule]) + r)
                    case null =>
                      acc3
                case null =>
                  acc3

            case (acc3, _) =>
              acc3
          }
        case (acc2, _) =>
          acc2
      }
    case (acc1, _) =>
      acc1
  }

  def getEntitiesWithRestrictedRelations
  (allOntologies: Set[Ontology])
  : Map[Entity, Set[Relation]]
  = {
    val derivedRelations = getRelationsWithSWRLRules(allOntologies)
    allOntologies.foldLeft(Map.empty[Entity, Set[Relation]]) {
      case (acc1, o: Vocabulary) =>
        OmlRead.getStatements(o).asScala.foldLeft(acc1) {
          case (acc2, e: Entity) =>
            OmlSearch.findRelationRestrictions(e).asScala.foldLeft(acc2) {
              case (acc3, ax: RelationCardinalityRestrictionAxiom) =>
                ax.getKind match
                  case _@(CardinalityRestrictionKind.MIN | CardinalityRestrictionKind.EXACTLY) =>
                    val r = ax.getRelation
                    if derivedRelations.getOrElse(r, Set.empty[Rule]).isEmpty then
                      acc3.updated(e, acc3.getOrElse(e, Set.empty[Relation]) + r)
                    else
                      acc3

                  case _ =>
                    acc3

              case (acc3, ax: RelationRangeRestrictionAxiom) =>
                ax.getKind match
                  case RangeRestrictionKind.SOME =>
                    val r = ax.getRelation
                    if derivedRelations.getOrElse(r, Set.empty[Rule]).isEmpty then
                      acc3.updated(e, acc3.getOrElse(e, Set.empty[Relation]) + r)
                    else
                      acc3
                  case _ =>
                    acc3
            }

          case (acc2, _) =>
            acc2
        }

      case (acc1, _) =>
        acc1
    }
  }

  implicit def omlMemberOrdering[M <: Member]: Ordering[M]
  = new Ordering[M]:
    override def compare(x: M, y: M): Int =
      OmlRead.getIri(x).compare(OmlRead.getIri(y))

  def getTermSpecializations
  (allOntologies: Set[Ontology])
  : TransitiveClosure[SpecializableTerm] =
    val g: DiGraph[SpecializableTerm]
    = allOntologies.foldLeft(DiGraph.empty[SpecializableTerm]) {
      case (acc1, o: Vocabulary) =>
        OmlRead.getStatements(o).asScala.foldLeft(acc1) {
          case (acc2, s: SpecializableTerm) =>
            OmlRead.getGeneralTerms(s).asScala.foldLeft(acc2.addVertex(s)) {
              case (acc3, g) =>
                acc3.addVertex(g).addEdge(g, s)
            }
          case (acc2, _) =>
            acc2
         }
      case (acc1, _) =>
        acc1
    }
    g.transitiveClosure()

  def getPropertyTrees
  (allOntologies: Set[Ontology])
  : Map[Property, DiGraph[Property]]
  = {
    val gp = allOntologies.foldLeft(DiGraph.empty[Property]) {
      case (acc1, o: Vocabulary) =>
        OmlRead.getStatements(o).asScala.foldLeft(acc1) {
          case (acc2, p: Property) =>
            OmlRead.getGeneralTerms(p).asScala.foldLeft(acc2.addVertex(p)) {
              case (acc3, sp: Property) =>
                acc3.addVertex(sp).addEdge(sp, p)
              case (acc3, _) =>
                acc3
            }
          case (acc2, _) =>
            acc2
        }

      case (acc1, _) =>
        acc1
    }

    val dfs = gp.dfs()
    dfs.scc.values.foldLeft(Map.empty[Property, DiGraph[Property]]) {
      case (acc, component: DiGraph[Property]) =>
        val roots = component.vs.filter(component.parentsOf(_).isEmpty)
        assert(roots.size == 1)
        acc.updated(roots.head, component)
    }
  }

  def getPropertyTree
  (propertyTrees: Map[Property, DiGraph[Property]],
   property: Property)
  : DiGraph[Property]
  = propertyTrees.getOrElse(
      property,
      propertyTrees.values
        .find(g => g.vs.contains(property))
        .getOrElse(DiGraph.empty[Property]))

  def getRelationTrees
  (allOntologies: Set[Ontology])
  : Map[Relation, DiGraph[Relation]]
  = {
    val gp = allOntologies.foldLeft(DiGraph.empty[Relation]) {
      case (acc1, o: Vocabulary) =>
        OmlRead.getStatements(o).asScala.foldLeft(acc1) {
          case (acc2, re: RelationEntity) =>
            val of = Option.apply(re.getForwardRelation)
            val or = Option.apply(re.getReverseRelation)
            val acc3 = of.fold(acc2)(acc2.addVertex)
            val acc4 = or.fold(acc3)(acc3.addVertex)
            OmlRead.getGeneralTerms(re).asScala.foldLeft(acc4) {
              case (acc5, sre: RelationEntity) =>
                val osf = Option.apply(sre.getForwardRelation)
                val osr = Option.apply(sre.getReverseRelation)
                val acc6 = osf.fold(acc5) { sf =>
                  of.fold(acc5.addVertex(sf)) { f =>
                    acc5.addEdge(sf, f)
                  }
                }
                val acc7 = osr.fold(acc6) { sr =>
                  of.fold(acc6.addVertex(sr)) { r =>
                    acc6.addEdge(sr, r)
                  }
                }
                acc7
              case (acc5, _) =>
                acc5
            }
          case (acc2, _) =>
            acc2
        }

      case (acc1, _) =>
        acc1
    }

    val dfs = gp.dfs()
    dfs.scc.values.foldLeft(Map.empty[Relation, DiGraph[Relation]]) {
      case (acc, component: DiGraph[Relation]) =>
        val roots = component.vs.filter(component.parentsOf(_).isEmpty)
        assert(roots.size == 1)
        acc.updated(roots.head, component)
    }
  }

  def getRelationTree
  (relationTrees: Map[Relation, DiGraph[Relation]],
   relation: Relation)
  : DiGraph[Relation]
  = relationTrees.getOrElse(
    relation,
    relationTrees.values
      .find(g => g.vs.contains(relation))
      .getOrElse(DiGraph.empty[Relation]))

  def getEntityInstances
  (allOntologies: Set[Ontology],
   entities: Set[Entity],
   termSpecializations: TransitiveClosure[SpecializableTerm])
  : Map[Entity, Set[NamedInstance]]
  = entities.foldLeft(Map.empty[Entity, Set[NamedInstance]]) {
    case (mi, e) =>
      val specializations = termSpecializations.successors(e) + e
      val instances = specializations.foldLeft(Set.empty[NamedInstance]) {
        case (is, s: Concept) =>
          is ++ OmlSearch.findConceptInstancesWithType(s).asScala
        case (is, _) =>
          is
      }
      mi.updated(e, instances)
  }

  /**
   * Looking at the Java implementation of getScalarPropertyCounts, getStructuredPropertyCounts and getRelationCounts
   * one can notice similarities:
   * - all 3 involve a type argument used in several places (property or relation)
   *   This type is called PropertyOrRelationArgument
   * - all 3 involve a kind of property or relation (scalar/structured property or relation)
   *   This tyoe is called PropertyOrRelationKind
   * - all 3 involve counting the cardinality of property/relation values or instances
   *   This type is called ValueOrInstance
   *  
   * Furthermore, the generation of OWL cardinality restriction axioms is structurally identical
   * for scalar properties, structured properties and relations; which leads to a generalization
   * (see generateCardinalityRestrictions)
   *
   * The common logic for all 3 methods can be generalized with the help of two parameterized functions:
   * - how to get the tree of properties/relations for a given property or relation
   *   This is the method: getPropertyOrRelationTree
   *
   * - collecting all values or instances in the range of a property or relation for a given subject instance
   *   This is the method collectPropertyOrRelationValuesOrInstancesForSubject
   *
   * The common logic defined in getFeaturePropertyCounts is parameterized with an instance of the trait FeatureCounts
   */
  trait FeatureCounts:
    type PropertyOrRelationArgument <: (Property | Relation)
    type PropertyOrRelationKind <: (ScalarProperty | StructuredProperty | Relation) & PropertyOrRelationArgument
    type ValueOrInstance <: (Literal | StructureInstance | NamedInstance)

    def filterProperty(p: PropertyOrRelationArgument): Option[PropertyOrRelationKind]

    def getPropertyOrRelationTree
    (propertyOrRelationTrees: Map[PropertyOrRelationArgument, DiGraph[PropertyOrRelationArgument]],
     propertyOrRelation: PropertyOrRelationArgument)
    : DiGraph[PropertyOrRelationArgument]

    def collectPropertyOrRelationValuesOrInstancesForSubject
    (subj: NamedInstance,
     allPropertiesOrRelations: Set[PropertyOrRelationArgument])
    : Map[PropertyOrRelationKind, Set[ValueOrInstance]]

    def generateCardinalityRestrictions
    (owlOntology: OWLOntology,
     owlApi: OwlApi,
     instancePropertyOrRelationCounts: Map[NamedInstance, Map[PropertyOrRelationKind, Integer]])
    : Unit
    = instancePropertyOrRelationCounts.foreach { case (subj, counts) =>
      val subj_iri = IRI.create(OmlRead.getIri(subj))
      val ni: OWLNamedIndividual = owlApi.getOWLNamedIndividual(subj_iri)
      counts.foreach { case (prop, count) =>
        val prop_iri = IRI.create(OmlRead.getIri(prop))
        val op: OWLObjectProperty = owlApi.getOWLObjectProperty(prop_iri)
        val mc: OWLObjectMaxCardinality = owlApi.getOWLObjectMaxCardinality(count, op)
        val ax: OWLClassAssertionAxiom = owlApi.getOWLClassAssertionAxiom(mc, ni)
        val change = owlOntology.addAxiom(ax)
        assert(change != ChangeApplied.UNSUCCESSFULLY)
      }
    }

  /**
   *
   * @param fc An instance of the FeatureCounts traits providing two kinds of members:
   *           - types (e.g., PropertyOrRelationArgument)
   *           - methods (e.g., filterProperty)
   * @param entitiesWithRestrictions a map whose type depends on an fc type member.
   * @param entityInstances
   * @param termSpecializations
   * @param propertyOrRelationTrees a map whose type depends on an fc type member.
   * @tparam FC
   * @return counts of values or instances that are in the range of property or relations of instance subjects.
   *         note that the result type depends on an fc type member.
   */
  def getFeaturePropertyCounts[FC <: FeatureCounts]
  (fc: FC,
   entitiesWithRestrictions: Map[Entity, Set[fc.PropertyOrRelationKind]],
   entityInstances: Map[Entity, Set[NamedInstance]],
   termSpecializations: TransitiveClosure[SpecializableTerm],
   propertyOrRelationTrees: Map[fc.PropertyOrRelationArgument, DiGraph[fc.PropertyOrRelationArgument]])
  : Map[NamedInstance, Map[fc.PropertyOrRelationKind, Integer]]
  = entitiesWithRestrictions.foldLeft(Map.empty[NamedInstance, Map[fc.PropertyOrRelationKind, Integer]]) {
    case (result1, (entity, propertiesOrRelations)) =>

      val allPropertiesOrRelations: Set[fc.PropertyOrRelationArgument] = propertiesOrRelations.foldLeft(Set.empty) { case (acc, p) =>
        acc ++ fc.getPropertyOrRelationTree(propertyOrRelationTrees, p).vs
      }

      entityInstances.getOrElse(entity, Set.empty).foldLeft(result1) { case (result2, subj) =>

        val subj_vals: Map[fc.PropertyOrRelationKind, Set[fc.ValueOrInstance]] =
          fc.collectPropertyOrRelationValuesOrInstancesForSubject(subj, allPropertiesOrRelations)

        val subj_counts: Map[fc.PropertyOrRelationKind, Integer] = propertiesOrRelations.foldLeft(Map.empty) {
          case (acc1, prop) =>
            val propertyTree = fc.getPropertyOrRelationTree(propertyOrRelationTrees, prop)
            propertyTree.vs.foldLeft(acc1) {
              case (acc2, p) =>
                fc.filterProperty(p) match
                  case Some(sp: fc.PropertyOrRelationKind) =>
                    val vals: Set[fc.ValueOrInstance] = propertyTree.childrenOf(sp).foldLeft(Set.empty) {
                      case (acc, q) =>
                        fc.filterProperty(q) match
                          case Some(sq: fc.PropertyOrRelationKind) =>
                            acc ++ subj_vals.getOrElse(sq, Set.empty)
                          case _ =>
                            acc
                    }
                    acc2.updated(sp, vals.size)
                  case _ =>
                    acc2
              }
        }

        result2.updated(subj, subj_counts)
      }
  }

  case object ScalarFeatureCounts extends FeatureCounts:
    override type PropertyOrRelationArgument = Property
    override type PropertyOrRelationKind = ScalarProperty
    override type ValueOrInstance = Literal

    override def filterProperty(p: PropertyOrRelationArgument): Option[PropertyOrRelationKind] = p match
      case sp: ScalarProperty =>
        Some(sp)
      case _ =>
        None

    override def getPropertyOrRelationTree
    (propertyOrRelationTrees: Map[PropertyOrRelationArgument, DiGraph[PropertyOrRelationArgument]],
     propertyOrRelation: PropertyOrRelationArgument)
    : DiGraph[PropertyOrRelationArgument]
    = getPropertyTree(propertyOrRelationTrees, propertyOrRelation)

    override def collectPropertyOrRelationValuesOrInstancesForSubject
    (subj: NamedInstance,
     allPropertiesOrRelations: Set[PropertyOrRelationArgument])
    : Map[PropertyOrRelationKind, Set[ValueOrInstance]]
    = OmlSearch.findPropertyValueAssertions(subj).asScala.foldLeft(Map.empty) {
      case (acc, pva) =>
        (getPropertyOrRelation(pva), getValueOrInstance(pva)) match
          case (Some(prop), Some(v: ValueOrInstance)) if allPropertiesOrRelations.contains(prop) =>
            acc.updated(prop, acc.getOrElse(prop, Set.empty) + v)
          case (_, _) =>
            // ignore cases where either the property or the value is null.
            acc
    }

    def getPropertyOrRelation(pva: PropertyValueAssertion): Option[PropertyOrRelationKind] = pva match
      case spva: ScalarPropertyValueAssertion =>
        Option.apply(spva.getProperty)
      case _ =>
        None

    def getValueOrInstance(pva: PropertyValueAssertion): Option[ValueOrInstance] = pva match
      case spva: ScalarPropertyValueAssertion =>
        Option.apply(spva.getValue)
      case _ =>
        None

  def getScalarPropertyCounts
  (entitiesWithRestrictions: Map[Entity, Set[ScalarProperty]],
   entityInstances: Map[Entity, Set[NamedInstance]],
   termSpecializations: TransitiveClosure[SpecializableTerm],
   propertyOrRelationTrees: Map[Property, DiGraph[Property]])
  : Map[NamedInstance, Map[ScalarProperty, Integer]]
  = getFeaturePropertyCounts(
    ScalarFeatureCounts,
    entitiesWithRestrictions,
    entityInstances,
    termSpecializations,
    propertyOrRelationTrees)

  case object StructuredFeatureCounts extends FeatureCounts:
    override type PropertyOrRelationArgument = Property
    override type PropertyOrRelationKind = StructuredProperty
    override type ValueOrInstance = StructureInstance

    override def filterProperty(p: PropertyOrRelationArgument): Option[PropertyOrRelationKind] = p match
      case sp: StructuredProperty =>
        Some(sp)
      case _ =>
        None

    override def getPropertyOrRelationTree
    (propertyOrRelationTrees: Map[PropertyOrRelationArgument, DiGraph[PropertyOrRelationArgument]],
     propertyOrRelation: PropertyOrRelationArgument)
    : DiGraph[PropertyOrRelationArgument]
    = getPropertyTree(propertyOrRelationTrees, propertyOrRelation)

    override def collectPropertyOrRelationValuesOrInstancesForSubject
    (subj: NamedInstance,
     allPropertiesOrRelations: Set[PropertyOrRelationArgument])
    : Map[PropertyOrRelationKind, Set[ValueOrInstance]]
    = OmlSearch.findPropertyValueAssertions(subj).asScala.foldLeft(Map.empty) {
      case (acc, pva) =>
        (getPropertyOrRelation(pva), getValueOrInstance(pva)) match
          case (Some(prop), Some(v: ValueOrInstance)) if allPropertiesOrRelations.contains(prop) =>
            acc.updated(prop, acc.getOrElse(prop, Set.empty) + v)
          case (_, _) =>
            // ignore cases where either the property or the value is null.
            acc
    }

    def getPropertyOrRelation(pva: PropertyValueAssertion): Option[PropertyOrRelationKind] = pva match
      case spva: StructuredPropertyValueAssertion =>
        Option.apply(spva.getProperty)
      case _ =>
        None

    def getValueOrInstance(pva: PropertyValueAssertion): Option[ValueOrInstance] = pva match
      case spva: StructuredPropertyValueAssertion =>
        Option.apply(spva.getValue)
      case _ =>
        None

  def getStructuredPropertyCounts
  (entitiesWithRestrictions: Map[Entity, Set[StructuredProperty]],
   entityInstances: Map[Entity, Set[NamedInstance]],
   termSpecializations: TransitiveClosure[SpecializableTerm],
   propertyOrRelationTrees: Map[Property, DiGraph[Property]])
  : Map[NamedInstance, Map[StructuredProperty, Integer]]
  = getFeaturePropertyCounts(
    StructuredFeatureCounts,
    entitiesWithRestrictions,
    entityInstances,
    termSpecializations,
    propertyOrRelationTrees)

  case object RelationCounts extends FeatureCounts:
    override type PropertyOrRelationArgument = Relation
    override type PropertyOrRelationKind = Relation
    override type ValueOrInstance = NamedInstance

    override def filterProperty(p: PropertyOrRelationArgument): Option[PropertyOrRelationKind] = p match
      case sp: StructuredProperty =>
        Some(sp)
      case _ =>
        None

    override def getPropertyOrRelationTree
    (propertyOrRelationTrees: Map[PropertyOrRelationArgument, DiGraph[PropertyOrRelationArgument]],
     propertyOrRelation: PropertyOrRelationArgument)
    : DiGraph[PropertyOrRelationArgument]
    = getRelationTree(propertyOrRelationTrees, propertyOrRelation)

    override def collectPropertyOrRelationValuesOrInstancesForSubject
    (subj: NamedInstance,
     allPropertiesOrRelations: Set[PropertyOrRelationArgument])
    : Map[PropertyOrRelationKind, Set[ValueOrInstance]]
    = {
      val m1: Map[PropertyOrRelationKind, Set[ValueOrInstance]] =
        OmlIndex.findRelationInstancesWithSource(subj).asScala.foldLeft(Map.empty) {
          case (acc1, ri) =>
            ri.getOwnedTypes.asScala.foldLeft(acc1) { case (acc2, rta) =>
              val rel = rta.getType.getForwardRelation
              if allPropertiesOrRelations.contains(rel) then
                acc2.updated(rel, acc2.getOrElse(rel, Set.empty) ++ ri.getTargets.asScala.toSet)
              else
                acc2
            }
        }
      val m2 =
        OmlSearch.findLinkAssertionsWithSource(subj).asScala.foldLeft(m1) {
          case (acc, link) =>
            val rel = link.getRelation
            if allPropertiesOrRelations.contains(rel) then
              acc.updated(rel, acc.getOrElse(rel, Set.empty) + link.getTarget)
            else
              acc
        }
      m2
    }

  def getRelationCounts
  (entitiesWithRestrictions: Map[Entity, Set[Relation]],
   entityInstances: Map[Entity, Set[NamedInstance]],
   termSpecializations: TransitiveClosure[SpecializableTerm],
   propertyOrRelationTrees: Map[Relation, DiGraph[Relation]])
  : Map[NamedInstance, Map[Relation, Integer]]
  = getFeaturePropertyCounts(
    RelationCounts,
    entitiesWithRestrictions,
    entityInstances,
    termSpecializations,
    propertyOrRelationTrees)

