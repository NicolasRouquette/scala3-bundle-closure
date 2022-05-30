import io.opencaesar.graph.*
import io.opencaesar.graph.ClassExpression.*
import io.opencaesar.graph.DFS.*

import scala.collection.immutable.SortedSet
import org.junit.Assert._
import org.junit.Test

class TestSimpleDiamond:

  val a = Singleton("a")
  val b = Singleton("b")
  val c = Singleton("c")
  val d = Singleton("d")

  val bd = Difference(b, d)
  val cd = Difference(c, d)

  val expected: DiGraph[ClassExpression[String]] =
      DiGraph.empty
        .addVertex(a)
        .addVertex(d)
        .addVertex(bd)
        .addVertex(cd)
        .addEdge(a, bd)
        .addEdge(a, d)
        .addEdge(a, cd)

  @Test def test(): Unit =
    val g1: DiGraph[ClassExpression[String]] =
      DiGraph.empty
        .addVertex(a)
        .addVertex(b)
        .addVertex(c)
        .addVertex(d)
        .addEdge(a,b)
        .addEdge(b,d)
        .addEdge(a,c)
        .addEdge(c,d)

    val g2 = g1.treeify()
    assertEquals(g2, expected)

