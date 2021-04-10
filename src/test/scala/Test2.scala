import io.opencaesar.graph.*

import org.junit.Assert._
import org.junit.Test

class Test2:

  @Test def t2(): Unit =
    
    val g1 = DiGraph
      .empty[String]
      .addVertex("undershorts")
      .addVertex("pants")
      .addVertex("belt")
      .addVertex("shirt")
      .addVertex("tie")
      .addVertex("jacket")
      .addVertex("socks")
      .addVertex("shoes")
      .addVertex("watch")

    val g2 = g1
      .addEdge("undershorts", "shoes")
      .addEdge("undershorts", "pants")
      .addEdge("pants", "shoes")
      .addEdge("pants", "belt")
      .addEdge("belt", "jacket")
      .addEdge("shirt", "belt")
      .addEdge("shirt", "tie")
      .addEdge("tie", "jacket")
      .addEdge("socks", "shoes")
    
    import DFS.dfs

    val result = g2.dfs()
    println(result.topo)
    
    val ri = g2.ri()
    println(ri)
