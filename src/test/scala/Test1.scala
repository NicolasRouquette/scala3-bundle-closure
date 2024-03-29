import io.opencaesar.graph.*

import org.junit.Test
import org.junit.Assert._

class Test1:

  @Test def test1(): Unit =
    
    val g1 = DiGraph
      .empty[String]
      .addVertex("u")
      .addVertex("v")
      .addVertex("w")
      .addVertex("x")
      .addVertex("y")
      .addVertex("z")
    
    assertEquals(6, g1.vs.size)
    
    val g2 = g1
      .addEdge("u", "v")
      .addEdge("u", "x")
      .addEdge("x", "v")
      .addEdge("v", "y")
      .addEdge("y", "x")
      .addEdge("w", "y")
      .addEdge("w", "z")
      .addEdge("z", "z")
    
    assertEquals(8, g2.es.size)

    import DFS.dfs

    val topo = g2.dfs()
    println(topo)
