import io.opencaesar.graph
import io.opencaesar.graph.Color
import io.opencaesar.graph.DFS.*
import io.opencaesar.graph.DiGraph
import org.junit.Assert._
import org.junit.Test

import scala.collection.immutable.SortedSet

class TestSCC:

  @Test def t1(): Unit =

    val g = DiGraph
      .empty[String]
      .addVertex("a")
      .addVertex("b")
      .addVertex("c")
      .addVertex("d")
      .addVertex("e")
      .addVertex("f")
      .addVertex("g")
      .addVertex("h")
      .addEdge("a", "b")
      .addEdge("b", "c")
      .addEdge("b", "e")
      .addEdge("b", "f")
      .addEdge("c", "d")
      .addEdge("c", "g")
      .addEdge("d", "c")
      .addEdge("d", "h")
      .addEdge("e", "a")
      .addEdge("e", "f")
      .addEdge("f", "g")
      .addEdge("g", "f")
      .addEdge("g", "h")
      .addEdge("h", "h")

    val (i1, s0) = dfsInit(g.vs)
    val s1 = s0.copy(colors = s0.colors.updated("c", Color.Gray))
    val (i2, s2) = g.dfsVisit("c", g.childrenOf("c"), i1, s1)
    println(i2)
    println(s2)
    val (i3, s3) = g.dfsVisit("b", g.childrenOf("b"), i2, s2)
    println(i3)
    println(s3)


  // https://www.geeksforgeeks.org/tarjan-algorithm-find-strongly-connected-components/
  @Test def t2(): Unit =

    val g = DiGraph
      .empty[String]
      .addVertex("a")
      .addVertex("b")
      .addVertex("c")
      .addVertex("d")
      .addVertex("e")
      .addVertex("f")
      .addVertex("g")
      .addVertex("h")
      .addVertex("i")
      .addVertex("j")
      .addEdge("a", "b")
      .addEdge("b", "c")
      .addEdge("c", "a")
      .addEdge("c", "d")
      .addEdge("c", "e")
      .addEdge("d", "a")
      .addEdge("e", "f")
      .addEdge("f", "g")
      .addEdge("f", "h")
      .addEdge("g", "c")
      .addEdge("g", "e")
      .addEdge("h", "i")
      .addEdge("i", "f")
      .addEdge("i", "j")
      .addEdge("j", "f")


    val (i1, s0) = dfsInit(g.vs)
    val s1 = s0.copy(colors = s0.colors.updated("a", Color.Gray))
    val (i2, s2) = g.dfsVisit("a", g.childrenOf("a"), i1, s1)
    println(i2)
    println(s2)

  // https://www.geeksforgeeks.org/tarjan-algorithm-find-strongly-connected-components/
  @Test def g1(): Unit =
    val g = DiGraph
      .empty[Int]
      .addEdge(1, 0)
      .addEdge(0, 2)
      .addEdge(2, 1)
      .addEdge(0, 3)
      .addEdge(3, 4)
    val dfs = g.dfs()
    assertTrue(dfs.scc.size == 3)
    val scc0 = dfs.scc(0)
    assertTrue(scc0.vs == SortedSet(0, 1, 2))
    assertTrue(scc0.es == SortedSet((0,2), (1,0), (2,1)))
    val scc3 = dfs.scc(3)
    assertTrue(scc3.vs == SortedSet(3))
    val scc4 = dfs.scc(4)
    assertTrue(scc4.vs == SortedSet(4))
    assertTrue(scc4.es.isEmpty)

  // https://www.geeksforgeeks.org/tarjan-algorithm-find-strongly-connected-components/
  @Test def g2(): Unit =
    val g = DiGraph
      .empty[Int]
      .addEdge(0, 1)
      .addEdge(1, 2)
      .addEdge(2, 3)
    val dfs = g.dfs()
    assertTrue(dfs.scc.size == 4)
    val scc0 = dfs.scc(0)
    assertTrue(scc0.vs == SortedSet(0))
    assertTrue(scc0.es.isEmpty)
    val scc1 = dfs.scc(1)
    assertTrue(scc1.vs == SortedSet(1))
    assertTrue(scc1.es.isEmpty)
    val scc2 = dfs.scc(2)
    assertTrue(scc2.vs == SortedSet(2))
    assertTrue(scc2.es.isEmpty)
    val scc3 = dfs.scc(3)
    assertTrue(scc3.vs == SortedSet(3))
    assertTrue(scc3.es.isEmpty)

  // https://www.geeksforgeeks.org/tarjan-algorithm-find-strongly-connected-components/
  @Test def g3(): Unit =
    val g = DiGraph
      .empty[Int]
      .addEdge(0, 1)
      .addEdge(1, 2)
      .addEdge(2, 0)
      .addEdge(1, 3)
      .addEdge(1, 4)
      .addEdge(1, 6)
      .addEdge(3, 5)
      .addEdge(4, 5)
    val dfs = g.dfs()
    assertTrue(dfs.scc.size == 5)
    val scc0 = dfs.scc(0)
    assertTrue(scc0.vs == SortedSet(0, 1, 2))
    assertTrue(scc0.es == SortedSet((0, 1), (1, 2), (2, 0)))
    val scc1 = dfs.scc(3)
    assertTrue(scc1.vs == SortedSet(3))
    assertTrue(scc1.es.isEmpty)
    val scc2 = dfs.scc(4)
    assertTrue(scc2.vs == SortedSet(4))
    assertTrue(scc2.es.isEmpty)
    val scc3 = dfs.scc(5)
    assertTrue(scc3.vs == SortedSet(5))
    assertTrue(scc3.es.isEmpty)

  // https://www.geeksforgeeks.org/tarjan-algorithm-find-strongly-connected-components/
  @Test def g4(): Unit =
    val g = DiGraph
      .empty[Int]
      .addEdge(0, 1)
      .addEdge(0, 3)
      .addEdge(1, 2)
      .addEdge(1, 4)
      .addEdge(2, 0)
      .addEdge(2, 6)
      .addEdge(3, 2)
      .addEdge(4, 5)
      .addEdge(4, 6)
      .addEdge(5, 6)
      .addEdge(5, 7)
      .addEdge(5, 8)
      .addEdge(5, 9)
      .addEdge(6, 4)
      .addEdge(7, 9)
      .addEdge(8, 9)
      .addEdge(9, 8)
    val dfs = g.dfs()
    assertTrue(dfs.scc.size == 4)
    val scc0 = dfs.scc(0)
    assertTrue(scc0.vs == SortedSet(0, 1, 2, 3))
    assertTrue(scc0.es == SortedSet((0, 1), (0, 3), (1, 2), (2, 0), (3, 2)))
    val scc1 = dfs.scc(6)
    assertTrue(scc1.vs == SortedSet(4, 5, 6))
    assertTrue(scc1.es == SortedSet((4, 5), (4, 6), (5, 6), (6, 4)))
    val scc2 = dfs.scc(7)
    assertTrue(scc2.vs == SortedSet(7))
    assertTrue(scc2.es.isEmpty)
    val scc3 = dfs.scc(9)
    assertTrue(scc3.vs == SortedSet(8, 9))
    assertTrue(scc3.es == SortedSet((8, 9), (9, 8)))