import io.opencaesar.graph.*
import io.opencaesar.graph.Axiom.*
import io.opencaesar.graph.ClassExpression.*
import io.opencaesar.graph.DFS.*

import scala.collection.immutable.SortedSet
import org.junit.Assert._
import org.junit.Test

class TestSymmetric8:

  val a = Singleton("a")
  val b = Singleton("b")
  val c = Singleton("c")
  val d = Singleton("d")
  val e = Singleton("e")
  val f = Singleton("f")
  val g = Singleton("g")
  val h = Singleton("h")

  val bh = Difference(b, h)
  val ch = Difference(c, h)
  val eh = Difference(e, h)
  val fh = Difference(f, h)

  val expected: DiGraph[ClassExpression[String]] =
    DiGraph.empty
      .addVertex(a)
      .addVertex(d)
      .addVertex(g)
      .addVertex(h)
      .addVertex(bh)
      .addVertex(ch)
      .addVertex(eh)
      .addVertex(fh)
      .addEdge(a, bh)
      .addEdge(a, h)
      .addEdge(a, ch)
      .addEdge(bh, d)
      .addEdge(bh, eh)
      .addEdge(ch, fh)
      .addEdge(ch, g)

  val expectedDax: Set[ClassExpressionSetAxiom[String]] =
    Set.empty +
      DisjointClassesAxiom(SortedSet.empty + bh + ch + h) +
      DisjointClassesAxiom(SortedSet.empty + d + eh) +
      DisjointClassesAxiom(SortedSet.empty + fh + g)

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
        .addEdge(a, b)
        .addEdge(b, d)
        .addEdge(b, e)
        .addEdge(e, h)
        .addEdge(a, c)
        .addEdge(c, g)
        .addEdge(c, f)
        .addEdge(f, h)

    val g2 = g1.treeify()
    assertEquals(expected, g2)

    val dax = g2.generateClosureAxioms(Axiom.DISJOINT_CLASSES)
    assertEquals(expectedDax, dax)
