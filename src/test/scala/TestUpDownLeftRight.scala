import io.opencaesar.graph.*
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

  val expected: DiGraph[ClassExpression[String]] =
    DiGraph.empty
      .addEdge(t, ul)
      .addEdge(t, ur)
      .addEdge(t, dl)
      .addEdge(t, dr)
      .addEdge(t, Difference(u, Union(SortedSet.empty[ClassExpression[String]] + ul + ur)))
      .addEdge(t, Difference(d, Union(SortedSet.empty[ClassExpression[String]] + dl + dr)))
      .addEdge(t, Difference(l, Union(SortedSet.empty[ClassExpression[String]] + ul + dl)))
      .addEdge(t, Difference(r, Union(SortedSet.empty[ClassExpression[String]] + ur + dr)))

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
    assertEquals(g2, expected)