import io.opencaesar.graph.*
import io.opencaesar.graph.ClassExpression.*
import io.opencaesar.graph.DFS.*

import scala.collection.immutable.SortedSet
import org.junit.Assert.*
import org.junit.Test

class TestTaxonomyProperties:

  val a = Singleton("a")
  val b = Singleton("b")
  val c = Singleton("c")

  val notConnectedNotTree: DiGraph[ClassExpression[String]] =
    DiGraph.empty
      .addVertex(a)
      .addVertex(b)
      .addVertex(c)

  val connectedTree =
    notConnectedNotTree
      .addEdge(a, b)
      .addEdge(a, c)

  val connectedNotTree =
    connectedTree
      .addEdge(b, c)

  @Test
  def testConnected(): Unit =
    assertFalse(notConnectedNotTree.isConnected)
    assertTrue(connectedTree.isConnected)
    assertTrue(connectedNotTree.isConnected)

  @Test
  def testEnsureConnected(): Unit =
    try
      notConnectedNotTree.ensureConnected()
      fail("no UnconnectedTaxonomyException thrown")
    catch
      case _: UnconnectedTaxonomyException =>
        assertTrue(true)

    try
      connectedTree.ensureConnected()
      assertTrue(true)
    catch
      case _: UnconnectedTaxonomyException =>
        fail("UnconnectedTaxonomyException thrown")

    try
      connectedNotTree.ensureConnected()
      assertTrue(true)
    catch
      case _: UnconnectedTaxonomyException =>
        fail("UnconnectedTaxonomyException thrown")

  @Test
  def testIsTree(): Unit =
    assertFalse(notConnectedNotTree.isTree)
    assertTrue(connectedTree.isTree)
    assertFalse(connectedNotTree.isTree)

  @Test
  def testEnsureTree(): Unit =
    try
      notConnectedNotTree.ensureTree()
      fail("no InvalidTreeException thrown")
    catch
      case _: InvalidTreeException =>
        assertTrue(true)

    try
      connectedTree.ensureTree()
      assertTrue(true)
    catch
      case _: InvalidTreeException =>
        fail("InvalidTreeException thrown")

    try
      connectedNotTree.ensureTree()
      fail("no InvalidTreeException thrown")
    catch
      case _: InvalidTreeException =>
        assertTrue(true)
