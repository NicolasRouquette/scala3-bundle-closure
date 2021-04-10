package io.opencaesar.oml2owl

import io.opencaesar.oml.*
import io.opencaesar.oml.util.{OmlRead, OmlSearch}
import org.semanticweb.owlapi.model.OWLOntology
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

  def run(): Unit =
    val allOntologies = OmlRead.reflexiveClosure(o, OmlRead.getImportedOntologies).asScala.toSet

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
   * The java code for getScalarpropertyCounts and getStructuredPropertyCounts is very similar.
   * The differences pertain to the kind of property (scalar vs. structured)
   * and the kind of values they can have (literal vs. structured instance)
   *
   * The FeatureCounts trait allows for generalizing the logic of these two methods into one
   * where the differences are captured in the different implementations of the trait.
   */
  trait FeatureCounts:
    type PropertyKind <: FeatureProperty
    type PVA <: (ScalarPropertyValueAssertion | StructuredPropertyValueAssertion)
    type Value <: (Literal | StructureInstance)

    def filterProperty(p: Property): Option[PropertyKind]
    def getProperty(pva: PropertyValueAssertion): Option[PropertyKind]
    def getValue(pva: PropertyValueAssertion): Option[Value]

  def getFeaturePropertyCounts[FC <: FeatureCounts]
  (fc: FC,
   entitiesWithRestrictedProperties: Map[Entity, Set[fc.PropertyKind]],
   entityInstances: Map[Entity, Set[NamedInstance]],
   termSpecializations: TransitiveClosure[SpecializableTerm],
   propertyTrees: Map[Property, DiGraph[Property]])
  : Map[NamedInstance, Map[fc.PropertyKind, Integer]]
  = entitiesWithRestrictedProperties.foldLeft(Map.empty[NamedInstance, Map[fc.PropertyKind, Integer]]) {
    case (result1, (entity, properties)) =>

      val allProperties: Set[Property] = properties.foldLeft(Set.empty) { case (acc, p) =>
        acc ++ getPropertyTree(propertyTrees, p).vs
      }

      entityInstances.getOrElse(entity, Set.empty).foldLeft(result1) { case (result2, subj) =>

        val subj_vals: Map[fc.PropertyKind, Set[fc.Value]] =
          OmlSearch.findPropertyValueAssertions(subj).asScala.foldLeft(Map.empty) {
            case (acc, pva) =>
              (fc.getProperty(pva), fc.getValue(pva)) match
                case (Some(prop), Some(v: fc.Value)) =>
                  acc.updated(prop, acc.getOrElse(prop, Set.empty) + v)
                case (_, _) =>
                  // ignore cases where either the property or the value is null.
                  acc
          }

        val subj_counts: Map[fc.PropertyKind, Integer] = properties.foldLeft(Map.empty) {
          case (acc1, prop) =>
            val propertyTree = getPropertyTree(propertyTrees, prop)
            propertyTree.vs.foldLeft(acc1) {
              case (acc2, p) =>
                fc.filterProperty(p) match
                  case Some(sp: fc.PropertyKind) =>
                    val vals: Set[fc.Value] = propertyTree.childrenOf(sp).foldLeft(Set.empty) {
                      case (acc, q) =>
                        fc.filterProperty(q) match
                          case Some(sq: fc.PropertyKind) =>
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
    override type PropertyKind = ScalarProperty
    override type PVA = ScalarPropertyValueAssertion
    override type Value = Literal

    override def filterProperty(p: Property): Option[PropertyKind] = p match
      case sp: ScalarProperty =>
        Some(sp)
      case _ =>
        None

    override def getProperty(pva: PropertyValueAssertion): Option[PropertyKind] = pva match
      case spva: ScalarPropertyValueAssertion =>
        Option.apply(spva.getProperty)
      case _ =>
        None

    override def getValue(pva: PropertyValueAssertion): Option[Value] = pva match
      case spva: ScalarPropertyValueAssertion =>
        Option.apply(spva.getValue)
      case _ =>
        None

  def getScalarPropertyCounts
  (entitiesWithRestrictedProperties: Map[Entity, Set[ScalarProperty]],
   entityInstances: Map[Entity, Set[NamedInstance]],
   termSpecializations: TransitiveClosure[SpecializableTerm],
   propertyTrees: Map[Property, DiGraph[Property]])
  : Map[NamedInstance, Map[ScalarProperty, Integer]]
  = getFeaturePropertyCounts(
    ScalarFeatureCounts,
    entitiesWithRestrictedProperties,
    entityInstances,
    termSpecializations,
    propertyTrees)

  case object StructuredFeatureCounts extends FeatureCounts:
    override type PropertyKind = StructuredProperty
    override type PVA = StructuredPropertyValueAssertion
    override type Value = StructureInstance

    override def filterProperty(p: Property): Option[PropertyKind] = p match
      case sp: StructuredProperty =>
        Some(sp)
      case _ =>
        None

    override def getProperty(pva: PropertyValueAssertion): Option[PropertyKind] = pva match
      case spva: StructuredPropertyValueAssertion =>
        Option.apply(spva.getProperty)
      case _ =>
        None

    override def getValue(pva: PropertyValueAssertion): Option[Value] = pva match
      case spva: StructuredPropertyValueAssertion =>
        Option.apply(spva.getValue)
      case _ =>
        None

  def getStructuredPropertyCounts
  (entitiesWithRestrictedProperties: Map[Entity, Set[StructuredProperty]],
   entityInstances: Map[Entity, Set[NamedInstance]],
   termSpecializations: TransitiveClosure[SpecializableTerm],
   propertyTrees: Map[Property, DiGraph[Property]])
  : Map[NamedInstance, Map[StructuredProperty, Integer]]
  = getFeaturePropertyCounts(
    StructuredFeatureCounts,
    entitiesWithRestrictedProperties,
    entityInstances,
    termSpecializations,
    propertyTrees)

