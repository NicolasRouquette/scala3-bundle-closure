import io.opencaesar.graph.*
import io.opencaesar.graph.ClassExpression.*
import io.opencaesar.graph.DFS.*

import scala.collection.immutable.SortedSet
import org.junit.Assert.*
import org.junit.Test

class TestDoubleEdgeChainTaxonomy:

  val a = Singleton("a")
  val b = Singleton("b")
  val c = Singleton("c")

  val setA: SortedSet[ClassExpression[String]] = SortedSet(a)
  val setB: SortedSet[ClassExpression[String]] = SortedSet(b)
  val setAB: SortedSet[ClassExpression[String]] = SortedSet(a,b)
  val setC: SortedSet[ClassExpression[String]] = SortedSet(c)
  val setBC: SortedSet[ClassExpression[String]] = SortedSet(b,c)

  val taxonomyABC: DiGraph[ClassExpression[String]] =
    DiGraph.empty
    .addVertex(a)
    .addVertex(b)
    .addVertex(c)
    .addEdge(a,b)
    .addEdge(b,c)

  val taxonomyBC: DiGraph[ClassExpression[String]] =
    DiGraph.empty
      .addVertex(b)
      .addVertex(c)
      .addEdge(b,c)

  val taxonomyAC: DiGraph[ClassExpression[String]] =
    DiGraph.empty
      .addVertex(a)
      .addVertex(c)
      .addEdge(a,c)

  val taxonomyAB: DiGraph[ClassExpression[String]] =
    DiGraph.empty
      .addVertex(a)
      .addVertex(b)
      .addEdge(a,b)

  val taxonomyA: DiGraph[ClassExpression[String]] =
    DiGraph.empty
      .addVertex(a)

  val taxonomyC: DiGraph[ClassExpression[String]] =
    DiGraph.empty
      .addVertex(c)

  @Test
  def testChildrenOf(): Unit =
    assertEquals(setB, taxonomyABC.childrenOf(a))
    assertEquals(setC, taxonomyABC.childrenOf(b))
    assertTrue(taxonomyABC.childrenOf(c).isEmpty)

  @Test
  def testDirectChildrenOf(): Unit =
    assertEquals(setB, taxonomyABC.directChildrenOf(a))
    assertEquals(setC, taxonomyABC.directChildrenOf(b))
    assertTrue(taxonomyABC.directChildrenOf(c).isEmpty)

  @Test
  def testDescendantsOf(): Unit =
    assertEquals(setBC, taxonomyABC.descendantsOf(a))
    assertEquals(setC, taxonomyABC.descendantsOf(b))
    assertTrue(taxonomyABC.descendantsOf(c).isEmpty)

  @Test
  def testParentsOf(): Unit =
    assertTrue(taxonomyABC.parentsOf(a).isEmpty)
    assertEquals(setA, taxonomyABC.parentsOf(b))
    assertEquals(setB, taxonomyABC.parentsOf(c))

  @Test
  def testDirectParentsOf(): Unit =
    assertTrue(taxonomyABC.directParentsOf(a).isEmpty)
    assertEquals(setA, taxonomyABC.directParentsOf(b))
    assertEquals(setB, taxonomyABC.directParentsOf(c))

  @Test
  def testAncestorsOf(): Unit =
    assertTrue(taxonomyABC.ancestorsOf(a).isEmpty)
    assertEquals(setA, taxonomyABC.ancestorsOf(b))
    assertEquals(setAB, taxonomyABC.ancestorsOf(c))

  @Test
  def testMultiParentChild(): Unit =
    assertFalse(taxonomyABC.multiParentChild().isDefined)

  @Test
  def testExciseVertex(): Unit =
    assertEquals(taxonomyBC, taxonomyABC.exciseVertex(a))
    assertEquals(taxonomyAC, taxonomyABC.exciseVertex(b))
    assertEquals(taxonomyAB, taxonomyABC.exciseVertex(c))

  @Test
  def testExciseVertices(): Unit =
    assertEquals(taxonomyA, taxonomyABC.exciseVertices(setBC))

  @Test
  def testExciseVerticesIf(): Unit =
    assertEquals(taxonomyA, taxonomyABC.exciseVerticesIf(v => setBC.contains(v)))

  @Test
  def testRootAt(): Unit =
    assertEquals(taxonomyABC, taxonomyBC.rootAt(a))
    assertEquals(taxonomyABC, taxonomyC.rootAt(b).rootAt(a))

  @Test
  def testTransitiveReduction(): Unit =
    val t1 = taxonomyABC.addEdge(a, c)
    assertEquals(taxonomyABC, taxonomyABC.transitiveReduction())
    assertEquals(taxonomyABC, t1.transitiveReduction())

  @Test
  def testTreeify(): Unit =
    assertEquals(taxonomyABC, taxonomyABC.treeify())

