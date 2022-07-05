
import io.opencaesar.graph.ClassExpression.Singleton
import io.opencaesar.graph.{ClassExpression, DiGraph, KosarajuStrongConnectivityInspector}
import org.junit.Test

import scala.collection.immutable.SortedSet

/**
 * See org.jgrapht.alg.connectivity.StrongConnectivityAlgorithmTest
 */
class StrongConnectivityTest:

  val V1: ClassExpression[String] = Singleton("V1")
  val V2: ClassExpression[String] = Singleton("V2")
  val V3: ClassExpression[String] = Singleton("V3")
  val V4: ClassExpression[String] = Singleton("V4")
  val V5: ClassExpression[String] = Singleton("V5")
  val V6: ClassExpression[String] = Singleton("V6")
  val V7: ClassExpression[String] = Singleton("V7")
  val V8: ClassExpression[String] = Singleton("V8")
  val V9: ClassExpression[String] = Singleton("V9")
  val V10: ClassExpression[String] = Singleton("V10")

  @Test def testStronglyConnected1(): Unit =
    val g: DiGraph[ClassExpression[String]] =
      DiGraph.empty
        .addEdge(V1, V2)
        .addEdge(V2, V1)
        .addEdge(V3, V4)

    assertStronglyConnectedSets(g,
      SortedSet.empty[ClassExpression[String]] + V1 + V2,
      SortedSet.empty[ClassExpression[String]] + V3,
      SortedSet.empty[ClassExpression[String]] + V4)

  @Test def testStronglyConnected2(): Unit =
    val g: DiGraph[ClassExpression[String]] =
      DiGraph.empty
        .addEdge(V1, V2)
        .addEdge(V2, V1)
        .addEdge(V4, V3)
        .addEdge(V3, V2)

    assertStronglyConnectedSets(g,
      SortedSet.empty[ClassExpression[String]] + V1 + V2,
      SortedSet.empty[ClassExpression[String]] + V3,
      SortedSet.empty[ClassExpression[String]] + V4)

  @Test def testStronglyConnected3(): Unit =
    val g: DiGraph[ClassExpression[String]] =
      DiGraph.empty
        .addEdge(V1, V2)
        .addEdge(V2, V3)
        .addEdge(V3, V1)
        .addEdge(V1, V4)
        .addEdge(V1, V4)
        .addEdge(V2, V4)
        .addEdge(V3, V4)

    assertStronglyConnectedSets(g,
      SortedSet.empty[ClassExpression[String]] + V1 + V2 + V3,
      SortedSet.empty[ClassExpression[String]] + V4)

  @Test def testStronglyConnected5(): Unit =
    val g: DiGraph[ClassExpression[String]] =
      DiGraph.empty
        .addEdge(V1, V2)
        .addEdge(V1, V3)

        .addEdge(V2, V3)
        .addEdge(V2, V4)

        .addEdge(V4, V3)
        .addEdge(V4, V5)

        .addEdge(V5, V2)
        .addEdge(V5, V6)

        .addEdge(V6, V3)
        .addEdge(V6, V4)

    assertStronglyConnectedSets(g,
      SortedSet.empty[ClassExpression[String]] + V1,
      SortedSet.empty[ClassExpression[String]] + V2 + V4 + V5 + V6,
      SortedSet.empty[ClassExpression[String]] + V3)

  @Test def testStronglyConnected6(): Unit =
    val g: DiGraph[ClassExpression[String]] =
      DiGraph.empty
        .addEdge(V1, V5)

        .addEdge(V2, V1)

        .addEdge(V3, V2)
        .addEdge(V3, V4)

        .addEdge(V4, V3)

        .addEdge(V5, V2)

        .addEdge(V6, V2)
        .addEdge(V6, V5)
        .addEdge(V6, V7)

        .addEdge(V7, V3)
        .addEdge(V7, V6)

        .addEdge(V8, V4)
        .addEdge(V8, V7)

    assertStronglyConnectedSets(g,
      SortedSet.empty[ClassExpression[String]] + V1 + V2 + V5,
      SortedSet.empty[ClassExpression[String]] + V3 + V4,
      SortedSet.empty[ClassExpression[String]] + V6 + V7,
      SortedSet.empty[ClassExpression[String]] + V8)

  @Test def testStronglyConnected7(): Unit =
    val g: DiGraph[ClassExpression[String]] =
      DiGraph.empty
        .addEdge(V1, V2)

        .addEdge(V2, V3)

        .addEdge(V3, V4)
        .addEdge(V3, V5)

        .addEdge(V4, V1)

        .addEdge(V5, V3)

    assertStronglyConnectedSets(g,
      SortedSet.empty[ClassExpression[String]] + V1 + V2 + V3 + V4 + V5)

  @Test def testStronglyConnected8(): Unit =
    val g: DiGraph[ClassExpression[String]] =
      DiGraph.empty
        .addEdge(V1, V2)
        .addEdge(V1, V4)

        .addEdge(V2, V3)
        .addEdge(V2, V5)

        .addEdge(V3, V1)
        .addEdge(V3, V7)

        .addEdge(V4, V3)

        .addEdge(V5, V6)
        .addEdge(V5, V7)

        .addEdge(V6, V7)
        .addEdge(V6, V8)
        .addEdge(V6, V9)
        .addEdge(V6, V10)

        .addEdge(V7, V5)

        .addEdge(V8, V10)

        .addEdge(V9, V10)

        .addEdge(V10, V9)

    assertStronglyConnectedSets(g,
      SortedSet.empty[ClassExpression[String]] + V1 + V2 + V3 + V4,
      SortedSet.empty[ClassExpression[String]] + V5 + V6 + V7,
      SortedSet.empty[ClassExpression[String]] + V8,
      SortedSet.empty[ClassExpression[String]] + V9 + V10)

  def assertStronglyConnectedSets
  (g: DiGraph[ClassExpression[String]],
   sets: SortedSet[ClassExpression[String]]*): Unit =
    val sccs = KosarajuStrongConnectivityInspector(g).stronglyConnectedSets
    assert(sccs.size == sets.size, s"Expecting ${sets.size} components but found ${sccs.size}.")

    sccs.foreach { scc =>
      assert(sets.contains(scc), s"Unexpected SCC: ${scc.mkString(", ")}")
    }

    sets.foreach { set =>
      assert(sets.contains(set), s"Missing SCC: ${set.mkString(", ")}")
    }

  @Test def testCondensation(): Unit =
    val g: DiGraph[ClassExpression[String]] =
      DiGraph.empty
        .addEdge(V1, V2)
        .addEdge(V2, V1)
        .addEdge(V3, V4)
        .addEdge(V5, V4)

    val inspector: KosarajuStrongConnectivityInspector[ClassExpression[String]] =
      KosarajuStrongConnectivityInspector(g)

    val condensation: DiGraph[DiGraph[ClassExpression[String]]] =
      inspector.getCondensation

    val actual: Set[SortedSet[ClassExpression[String]]] =
      condensation.vs.foldLeft(Set.empty[SortedSet[ClassExpression[String]]]) { case (acc, g) =>
        acc + g.vs
    }

    val expected: Set[SortedSet[ClassExpression[String]]] =
      Set.empty[SortedSet[ClassExpression[String]]] +
        (SortedSet.empty[ClassExpression[String]] + V1 + V2) +
        (SortedSet.empty[ClassExpression[String]] + V3) +
        (SortedSet.empty[ClassExpression[String]] + V4) +
        (SortedSet.empty[ClassExpression[String]] + V5)

    assert(expected == actual)

    val g1 = getOnlyGraphWithVertices(condensation, SortedSet.empty[ClassExpression[String]] + V1 + V2)
    val g2 = getOnlyGraphWithVertices(condensation, SortedSet.empty[ClassExpression[String]] + V3)
    val g3 = getOnlyGraphWithVertices(condensation, SortedSet.empty[ClassExpression[String]] + V4)
    val g4 = getOnlyGraphWithVertices(condensation, SortedSet.empty[ClassExpression[String]] + V5)

    assert(g1.es == SortedSet.empty[(ClassExpression[String], ClassExpression[String])] + (V1 -> V2) + (V2 -> V1))
    assert(g2.es == SortedSet.empty[(ClassExpression[String], ClassExpression[String])])
    assert(g3.es == SortedSet.empty[(ClassExpression[String], ClassExpression[String])])
    assert(g4.es == SortedSet.empty[(ClassExpression[String], ClassExpression[String])])

    assert(condensation.es ==
      Set.empty[(DiGraph[ClassExpression[String]], DiGraph[ClassExpression[String]])] +
        (g2 -> g3) +
        (g4 -> g3))

  @Test def testCondensation2(): Unit =
    val g: DiGraph[ClassExpression[String]] =
      DiGraph.empty
        .addEdge(V1, V2)
        .addEdge(V2, V1)

        .addEdge(V3, V4)
        .addEdge(V4, V3)

        .addEdge(V1, V3)
        .addEdge(V2, V4)

    val inspector: KosarajuStrongConnectivityInspector[ClassExpression[String]] =
      KosarajuStrongConnectivityInspector(g)

    val condensation: DiGraph[DiGraph[ClassExpression[String]]] =
      inspector.getCondensation

    val actual: Set[SortedSet[ClassExpression[String]]] =
      condensation.vs.foldLeft(Set.empty[SortedSet[ClassExpression[String]]]) { case (acc, g) =>
        acc + g.vs
      }

    val expected: Set[SortedSet[ClassExpression[String]]] =
      Set.empty[SortedSet[ClassExpression[String]]] +
        (SortedSet.empty[ClassExpression[String]] + V1 + V2) +
        (SortedSet.empty[ClassExpression[String]] + V3 + V4)

    assert(expected == actual)

    val g1 = getOnlyGraphWithVertices(condensation, SortedSet.empty[ClassExpression[String]] + V1 + V2)
    val g2 = getOnlyGraphWithVertices(condensation, SortedSet.empty[ClassExpression[String]] + V3 + V4)

    assert(g1.es == SortedSet.empty[(ClassExpression[String], ClassExpression[String])] + (V1 -> V2) + (V2 -> V1))
    assert(g2.es == SortedSet.empty[(ClassExpression[String], ClassExpression[String])] + (V3 -> V4) + (V4 -> V3))

    assert(condensation.es ==
      Set.empty[(DiGraph[ClassExpression[String]], DiGraph[ClassExpression[String]])] +
        (g1 -> g2))

  def getOnlyGraphWithVertices
  (condensation: DiGraph[DiGraph[ClassExpression[String]]],
   vs: SortedSet[ClassExpression[String]])
  : DiGraph[ClassExpression[String]] =
    val matches = condensation.vs.filter(g => g.vs == vs)
    assert(matches.size == 1)
    matches.head
