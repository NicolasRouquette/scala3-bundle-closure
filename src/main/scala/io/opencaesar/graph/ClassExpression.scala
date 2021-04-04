package io.opencaesar.graph

import math.Ordered.orderingToOrdered

import scala.collection.immutable.SortedSet

sealed trait ClassExpression[O : Ordering] extends Ordered[ClassExpression[O]]:

  import ClassExpression.*

  def complement(): ClassExpression[O] =
    Complement(this)

  def difference(e: ClassExpression[O]): ClassExpression[O] =
    if this == e then
      Empty[O]()
    else
      e match
        case _: Empty[O] =>
          this
        case _: Universal[O] =>
          Empty[O]()
        case _ =>
          Difference[O](this, e)

  def intersection(e: ClassExpression[O]): ClassExpression[O] =
    if this == e then
      this
    else
      e match
        case _ @ ( _: Empty[O] | _: Intersection[O] | _: Universal[O] ) =>
          e.intersection(this)
        case _ =>
          Intersection[O](SortedSet[ClassExpression[O]](this, e))

  def union(e: ClassExpression[O]): ClassExpression[O] =
    if this == e then
      this
    else
      e match
        case _ @ ( _: Empty[O] | _: Union[O] | _: Universal[O] ) =>
          e.union(this)
        case _ =>
          Union[O](SortedSet[ClassExpression[O]](this, e))

  def toAtom(): String = "(" + toString() + ")"

object ClassExpression:

  given classExpressionDiff[O: Ordering]: HasDifference[ClassExpression[O]] with

    extension (x: ClassExpression[O]) def difference(y: ClassExpression[O]): ClassExpression[O] =
      x.difference(y)

  case class Universal[O : Ordering]() extends ClassExpression[O]:

    override def complement(): ClassExpression[O] = 
      Empty[O]()

    override def intersection(e: ClassExpression[O]): ClassExpression[O] =
      e
    
    override def union(e: ClassExpression[O]): ClassExpression[O] =
      this

    override def toAtom(): String = toString()

    override def toString(): String = "U"

    override def compare(that: ClassExpression[O]): Int =
      that match
        case _: Universal[O] =>
          0
        case _ =>
          1

  case class Empty[O : Ordering]() extends ClassExpression[O]:

    override def complement(): ClassExpression[O] =
      Universal[O]()

    override def difference(e: ClassExpression[O]): ClassExpression[O] =
      this

    override def intersection(e: ClassExpression[O]): ClassExpression[O] =
      this
    
    override def union(e: ClassExpression[O]): ClassExpression[O] =
      e

    override def toAtom(): String = toString()

    override def toString(): String = "∅"

    override def compare(that: ClassExpression[O]): Int =
      that match
        case _: Empty[O] =>
          0
        case _ =>
          -1

  case class Singleton[O : Ordering](o: O) extends ClassExpression[O]:

    override def toAtom(): String = toString()

    override def toString(): String = o.toString()

    override def compare(that: ClassExpression[O]): Int =
      that match
        case _: Empty[O] =>
          1
        case Singleton(x) =>
          o.compare(x)
        case _ =>
          -1

  sealed trait Unary[O : Ordering] extends ClassExpression[O]:
    
    val e: ClassExpression[O]

  case class Complement[O : Ordering](override val e: ClassExpression[O]) extends Unary[O]:

    override def complement(): ClassExpression[O] =
      e

    override def toAtom(): String = toString()

    override def toString(): String = e.toAtom() + "′"

    override def compare(that: ClassExpression[O]): Int =
      that match
        case _ @ ( _: Empty[O] | _: Singleton[O] ) =>
          1
        case Complement(f) =>
          e.compare(f)
        case _ =>
          -1

  sealed trait Binary[O : Ordering] extends ClassExpression[O]:

    val a, b: ClassExpression[O]

    def toString(op: String): String = 
      a.toAtom() + op + b.toAtom()

  case class Difference[O : Ordering](
    override val a: ClassExpression[O],
    override val b: ClassExpression[O]) extends Binary[O]:

    override def difference(e: ClassExpression[O]): ClassExpression[O] =
      if this == e then
        Empty[O]()
      else
        e match
          case _: Empty[O] =>
            this
          case _: Universal[O] =>
            Empty[O]()
          case _ =>
            Difference[O](a, b.union(e))

    override def toString(): String =
      toString("\\")

    override def compare(that: ClassExpression[O]): Int =
      that match
        case _ @ ( _: Empty[O] | _: Singleton[O] | _: Complement[O] )  =>
          1
        case Difference(u,v) =>
          a.compare(u) match
            case 0 =>
              v.compare(b)
            case x =>
              x
        case _ =>
          -1

  sealed trait NAry[O : Ordering] extends ClassExpression[O]:

    val s: SortedSet[ClassExpression[O]]

    def toString(c: String): String =
      // TODO: why is .toSeq necessary?
      s.toSeq.map(_.toString()).mkString(c)

    override def toAtom(): String =
      if s.size <= 1 then
        toString()
      else
        super.toAtom()

    def compare(other: SortedSet[ClassExpression[O]]): Int =
      (s.headOption, other.headOption) match
        case (Some(x), Some(y)) =>
          x.compare(y)
        case (Some(_), None) =>
          1
        case (None, Some(_)) =>
          -1
        case _ =>
          0

  case class Intersection[O : Ordering](override val s: SortedSet[ClassExpression[O]]) extends NAry[O]:

    override def intersection(e: ClassExpression[O]): ClassExpression[O] =
      e match
        case Intersection(t) =>
          Intersection[O](s ++ t)
        case _ =>
          Intersection[O](s + e)

    override def toString(): String =
      toString("∩")

    override def compare(that: ClassExpression[O]): Int =
      that match
        case _ @ ( _: Empty[O] | _: Singleton[O] | _: Complement[O] | _: Difference[O] )  =>
          1
        case Intersection(other) =>
          compare(other)
        case _ =>
          -1

  case class Union[O : Ordering](override val s: SortedSet[ClassExpression[O]]) extends NAry[O]:

    override def union(e: ClassExpression[O]): ClassExpression[O] =
      e match
        case Union(t) =>
          Union[O](s ++ t)
        case _ =>
          Union[O](s + e)

    override def toString(): String =
      toString("∪")

    override def compare(that: ClassExpression[O]): Int =
      that match
        case Union(other) =>
          compare(other)
        case _: Universal[O] =>
          -1
        case _ =>
          1
