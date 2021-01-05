package io.opencaesar.graph

import scala.collection.immutable.SortedSet
import org.junit.Assert._
import org.junit.Test

class Test3 {
  @Test def t3(): Unit = {

    val g1 = DiGraph
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
      .addVertex("k")
      .addVertex("l")
      .addVertex("m")
      .addVertex("n")
      .addVertex("o")

    val g2 = g1
      .addEdge("a", "b")
      .addEdge("a", "e")
      .addEdge("a", "f")
      .addEdge("b", "c")
      .addEdge("b", "g")
      .addEdge("c", "j")
      .addEdge("d", "j")
      .addEdge("e", "d")
      .addEdge("f", "h")
      .addEdge("g", "j")
      .addEdge("h", "i")
      .addEdge("i", "j")
      .addEdge("j", "k")
      .addEdge("j", "l")
      .addEdge("j", "m")
      .addEdge("j", "n")
      .addEdge("k", "o")
      .addEdge("l", "o")
      .addEdge("m", "o")
      .addEdge("n", "o")
    // Redundant edges
      .addEdge("a", "h")
      .addEdge("g", "m")
      .addEdge("e", "n")
      .addEdge("c", "l")

    println(s"G: ${g2.es}")
    val gtr = TR_B.transitiveReduction(g2)

    println(s"Gtr: ${gtr.es}")
    assertEquals(gtr.es, 
      SortedSet[(String,String)](
        ("a","b"), 
        ("a","e"),
        ("a","f"), 
        ("b","c"), 
        ("b","g"), 
        ("c","j"), 
        ("d","j"), 
        ("e","d"),
        ("f","h"), 
        ("g","j"), 
        ("h","i"), 
        ("i","j"), 
        ("j","k"),
        ("j","l"),
        ("j","m"), 
        ("j","n"), 
        ("k","o"), 
        ("l","o"), 
        ("m","o"),
        ("n","o")
      ))
  }
}