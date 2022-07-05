import io.opencaesar.graph.*
import io.opencaesar.graph.Axiom.*
import io.opencaesar.graph.ClassExpression.*
import io.opencaesar.graph.DFS.*

import scala.collection.immutable.SortedSet
import org.junit.Assert._
import org.junit.Test

class TestAntiSymmetric:

  val a = Singleton("a")
  val b = Singleton("b")
  val c = Singleton("c")
  val d = Singleton("d")
  val e = Singleton("e")
  val f = Singleton("f")
  val g = Singleton("g")
  val h = Singleton("h")
  val i = Singleton("i")
  val j = Singleton("j")

  val ei = Difference(e, i)
  val ci = Difference(c, i)

  val expected: DiGraph[ClassExpression[String]] =
    DiGraph.empty
      .addVertex(a)
      .addVertex(b)
      .addVertex(ci)
      .addVertex(d)
      .addVertex(ei)
      .addVertex(f)
      .addVertex(g)
      .addVertex(h)
      .addVertex(i)
      .addVertex(j)
      .addEdge(a, b)
      .addEdge(b, d)
      .addEdge(b, ei)
      .addEdge(ei, h)
      .addEdge(b, i)
      .addEdge(i, j)
      .addEdge(a, ci)
      .addEdge(ci, f)
      .addEdge(ci, g)

  val expectedDax: Set[ClassExpressionSetAxiom[String]] =
    Set.empty[ClassExpressionSetAxiom[String]] +
      DisjointClassesAxiom(SortedSet.empty[ClassExpression[String]] + b + ci) +
      DisjointClassesAxiom(SortedSet.empty[ClassExpression[String]] + d + ei + i) +
      DisjointClassesAxiom(SortedSet.empty[ClassExpression[String]] + f + g)

  @Test def test(): Unit =
    val g1: DiGraph[ClassExpression[String]] =
      DiGraph.empty
        .addVertex(a)
        .addVertex(b)
        .addVertex(c)
        .addVertex(d)
        .addVertex(e)
        .addVertex(f)
        .addVertex(g)
        .addVertex(h)
        .addVertex(i)
        .addVertex(j)
        .addEdge(a, b)
        .addEdge(b, d)
        .addEdge(b, e)
        .addEdge(e, h)
        .addEdge(e, i)
        .addEdge(i, j)
        .addEdge(a, c)
        .addEdge(c, i)
        .addEdge(c, f)
        .addEdge(c, g)

    val g2 = g1.treeify()
    assertEquals(expected, g2)

    val dax = g2.generateClosureAxioms(Axiom.DISJOINT_CLASSES)
    assertEquals(expectedDax, dax)
