package io.opencaesar.graph

import scala.collection.immutable.SortedSet

sealed trait ClassExpressionSetAxiom[O : Ordering] extends Ordered[ClassExpressionSetAxiom[O]]:

  val set: SortedSet[ClassExpression[O]]

  def toString(kind: String): String =
    // TODO: why is .toSeq necessary?
    set.toSeq.mkString(kind + "(", ", ", ")")

  def compare(other: SortedSet[ClassExpression[O]]): Int =
    (set.headOption, other.headOption) match
      case (Some(x), Some(y)) =>
        x.compare(y)
      case (Some(_), None) =>
        1
      case (None, Some(_)) =>
        -1
      case _ =>
        0

case class DisjointClassesAxiom[O : Ordering]
(override val set: SortedSet[ClassExpression[O]]) 
extends ClassExpressionSetAxiom[O]:

  override def toString(): String = toString("DisjointClasses")

  override def compare(that: ClassExpressionSetAxiom[O]): Int =
    that match
      case DisjointClassesAxiom(other) =>
        compare(other)
      case _ =>
        -1

case class EquivalentClassesAxiom[O : Ordering]
(override val set: SortedSet[ClassExpression[O]]) 
extends ClassExpressionSetAxiom[O]:

  override def toString(): String = toString("EquivalentClasses")

  override def compare(that: ClassExpressionSetAxiom[O]): Int =
    that match
      case _: DisjointClassesAxiom[O] =>
        1
      case EquivalentClassesAxiom(other) =>
        compare(other)
      case _ =>
        -1
    
case class DisjointUnionAxiom[O : Ordering]
( c: ClassExpression.Singleton[O],
  override val set: SortedSet[ClassExpression[O]])
extends ClassExpressionSetAxiom[O]:

  override def toString(): String = set.toSeq.mkString(s"DisjointUnion($c,", ", ", ")")

  override def compare(that: ClassExpressionSetAxiom[O]): Int =
    that match
      case DisjointUnionAxiom(d,other) =>
        c.compare(d) match
          case 0 =>
            compare(other)
          case x =>
            x
      case _ =>
        1
    