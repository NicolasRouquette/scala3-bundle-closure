import io.opencaesar.graph.*
import io.opencaesar.graph.Axiom.*
import io.opencaesar.graph.ClassExpression.*
import io.opencaesar.graph.DFS.*

import scala.collection.immutable.SortedSet
import org.junit.Assert._
import org.junit.Test

class TestUpDownLeftRight:

  val t = Singleton("t")
  val u = Singleton("u")
  val d = Singleton("d")
  val l = Singleton("l")
  val r = Singleton("r")
  val ul = Singleton("ul")
  val ur = Singleton("ur")
  val dl = Singleton("dl")
  val dr = Singleton("dr")

  val diff_u = Difference(u, Union(SortedSet.empty[ClassExpression[String]] + ul + ur))
  val diff_d = Difference(d, Union(SortedSet.empty[ClassExpression[String]] + dl + dr))
  val diff_l = Difference(l, Union(SortedSet.empty[ClassExpression[String]] + ul + dl))
  val diff_r = Difference(r, Union(SortedSet.empty[ClassExpression[String]] + ur + dr))

  val expected: DiGraph[ClassExpression[String]] =
    DiGraph.empty
      .addEdge(t, ul)
      .addEdge(t, ur)
      .addEdge(t, dl)
      .addEdge(t, dr)
      .addEdge(t, diff_u)
      .addEdge(t, diff_d)
      .addEdge(t, diff_l)
      .addEdge(t, diff_r)

  val expectedDax: Set[ClassExpressionSetAxiom[String]] =
    Set.empty +
      DisjointClassesAxiom(SortedSet.empty + ul + ur + dl + dr + diff_u + diff_d + diff_l + diff_r)

  @Test def test(): Unit =
    val g1: DiGraph[ClassExpression[String]] =
      DiGraph.empty
        .addEdge(t, u)
        .addEdge(u, ul)
        .addEdge(u, ur)
        .addEdge(t, d)
        .addEdge(d, dl)
        .addEdge(d, dr)
        .addEdge(t, l)
        .addEdge(l, ul)
        .addEdge(l, dl)
        .addEdge(t, r)
        .addEdge(r, ur)
        .addEdge(r, dr)

    val g2 = g1.treeify()
    assertEquals(expected, g2)

    val dax = g2.generateClosureAxioms(Axiom.DISJOINT_CLASSES)
    assertEquals(expectedDax, dax)