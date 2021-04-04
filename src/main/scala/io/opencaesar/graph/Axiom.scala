package io.opencaesar.graph

import io.opencaesar.graph.DFS.*

import scala.collection.immutable.{SortedSet, SortedMap}

enum Axiom:
  case DISJOINT_CLASSES, EQUIVALENT_CLASSES, DISJOINT_UNION

object Axiom:

  extension[O : Ordering](g: DiGraph[ClassExpression[O]])
    def generateClosureAxioms(axiomType: Axiom)
    (using ops: HasDifference[ClassExpression[O]])
    : SortedSet[ClassExpressionSetAxiom[O]] =
      g.ensureConnected()
      val t = g.treeify()
      t.ensureTree()
      val s: SortedMap[ClassExpression[O], SortedSet[ClassExpression[O]]] = t.siblingMap()
      s.foldLeft[SortedSet[ClassExpressionSetAxiom[O]]](SortedSet.empty[ClassExpressionSetAxiom[O]]) {
        case (acc, (c, cs)) =>
          axiomType match
            case DISJOINT_CLASSES =>
              acc + DisjointClassesAxiom(cs)
            case DISJOINT_UNION =>
              acc + (c match
                case s: ClassExpression.Singleton[O] =>
                  DisjointUnionAxiom[O](cs + s)
                case _ =>
                  DisjointClassesAxiom[O](cs)
              )
            case EQUIVALENT_CLASSES =>
              acc
      }