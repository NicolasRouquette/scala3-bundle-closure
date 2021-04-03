import io.opencaesar.graph.*
import io.opencaesar.graph.ClassExpression.*
import io.opencaesar.graph.DFS.*

import scala.collection.immutable.SortedSet
import org.junit.Assert.*
import org.junit.Test

object TestSingleEdgeTaxonomy:

  val a = Singleton("a")
  val b = Singleton("b")

  val setA: SortedSet[ClassExpression[String]] = SortedSet(a)
  val setB: SortedSet[ClassExpression[String]] = SortedSet(b)

  val tAB: DiGraph[ClassExpression[String]] =
    DiGraph.empty
      .addVertex(a)
      .addVertex(b)
      .addEdge(a,b)

  val tB: DiGraph[ClassExpression[String]] =
    DiGraph.empty
      .addVertex(b)

  @Test
  def testChildrenOf(): Unit =
    assertEquals(setB, tAB.out(a))
    assertTrue(tAB.out(b).isEmpty)

  @Test
  def testDirectChildrenOf(): Unit =
    assertEquals(setB, tAB.directChildrenOf(a))
    assertTrue(tAB.directChildrenOf(b).isEmpty)

  @Test
  def testDescendentsOf(): Unit =
    assertEquals(setB, tAB.descendantsOf(a))
    assertTrue(tAB.descendantsOf(b).isEmpty)

  @Test
  def testParentsOf(): Unit =
    assertEquals(setA, tAB.in(b))
    assertTrue(tAB.in(a).isEmpty)

  @Test
  def testDirectParentsOf(): Unit =
    assertEquals(setA, tAB.directParentsOf(b))
    assertTrue(tAB.directParentsOf(a).isEmpty)

  @Test
  def testAncestorsOf(): Unit =
    assertEquals(setA, tAB.ancestorsOf(b))
    assertTrue(tAB.ancestorsOf(a).isEmpty)

  @Test
  def testExciseVertex(): Unit =
    assertEquals(tB, tAB.exciseVertex(a))

  @Test
  def testExciseVertices(): Unit =
    assertEquals(tB, tAB.exciseVertices(setA))

  @Test
  def testExciseVerticesIf(): Unit =
    assertEquals(tB, tAB.exciseVerticesIf(v => v == a))

  @Test
  def testRootAt(): Unit =
    assertEquals(tAB, tB.rootAt(a))

  @Test
  def testMultiParentChild(): Unit =
    assertFalse(tAB.multiParentChild().isDefined)

  @Test
  def testTreeify(): Unit =
    assertEquals(tAB, tAB.treeify())