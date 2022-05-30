
import io.opencaesar.graph.Axiom.*
import io.opencaesar.graph.ClassExpression.*
import io.opencaesar.graph.DFS.*
import io.opencaesar.graph.*

import scala.collection.immutable.{SortedMap, SortedSet}
import org.junit.Assert.*
import org.junit.Test

class TestAsymmetricTaxonomy:

  val pattern1 = "([a-z])\\\\\\(([a-z])∪([a-z])\\)".r
  val pattern2 = "([a-z])\\\\([a-z])".r

  def string2ClassExpression(s: String): ClassExpression[String] =
    s match
      case pattern1(a,b,c) =>
        Singleton(a).difference(Singleton(b).union(Singleton(c)))
      case pattern2(a,b) =>
        Singleton(a).difference(Singleton(b))
      case _ =>
        Singleton(s)

  def taxonomyFromEdgeList(l: List[String]): DiGraph[ClassExpression[String]] =
    l.grouped(2).foldLeft(DiGraph.empty[ClassExpression[String]]) {
      case (acc, List(x, y)) =>
        acc.addEdge(string2ClassExpression(x), string2ClassExpression(y))
      case (acc, List(x)) =>
        acc.addVertex(string2ClassExpression(x))
      case (acc, _) =>
        acc
    }

  val initialEdgeSpec = List(
    "a", "b",
    "a", "c",
    "b", "d",
    "b", "e",
    "c", "f",
    "c", "g",
    "c", "i",
    "e", "h",
    "e", "i",
    "f", "k",
    "i", "j",
    "j", "k")

  val vertexMap: Map[String, ClassExpression[String]] =
    Map.empty ++
      initialEdgeSpec.toSet.map(v => v -> Singleton(v))

  val initialTaxonomy = taxonomyFromEdgeList(initialEdgeSpec)

  // redundant edges

  val redundantEdgeSpec = List(
    "a", "d",
    "a", "e",
    "a", "f",
    "a", "g",
    "a", "h",
    "a", "i",
    "a", "j",
    "a", "k",
    "b", "h",
    "b", "i",
    "b", "j",
    "c", "j",
    "b", "k",
    "c", "k",
    "e", "j",
    "e", "k",
    "i", "k")

  val redundantEdgeTaxonomy = taxonomyFromEdgeList(initialEdgeSpec ::: redundantEdgeSpec)

  // After exciseVertex(i)

  val afterExciseVertexEdgeSpec = List(
    "a", "b",
    "a", "c",
    "b", "d",
    "b", "e",
    "c", "f",
    "c", "g",
    "c", "j",
    "e", "h",
    "e", "j",
    "f", "k",
    "j", "k")

  val afterExciseVertexTaxonomy = taxonomyFromEdgeList(afterExciseVertexEdgeSpec)

  // After exciseVertices({b, d, e, f, g})

  val afterExciseVerticesEdgeSpec = List(
    "c", "f",
    "c", "i",
    "f", "k",
    "i", "j",
    "j", "k")

  val afterExciseVerticesTaxonomy = taxonomyFromEdgeList(afterExciseVerticesEdgeSpec)

  // After exciseVertices({b, d, e, f, g})

  val unrootedEdgeSpec = List(
    "b", "d",
    "b", "e",
    "c", "f",
    "c", "g",
    "c", "i",
    "e", "h",
    "e", "i",
    "f", "k",
    "i", "j",
    "j", "k")

  val unrootedTaxonomy = taxonomyFromEdgeList(unrootedEdgeSpec)

  // After bypass(i, c)

  val afterBypassOneEdgeSpec = List(
    "a", "b",
    "a", "c",
    "b", "d",
    "b", "e",
    "c", "f",
    "c", "g",
    "a", "i",
    "e", "h",
    "e", "i",
    "f", "k",
    "i", "j",
    "j", "k")

  val afterBypassOneTaxonomy = taxonomyFromEdgeList(afterBypassOneEdgeSpec)

  // After bypass(i, {c, e})

  val afterBypassAllEdgeSpec = List("a", "b",
    "a", "c",
    "a", "i",
    "b", "d",
    "b", "e",
    "b", "i",
    "c", "f",
    "c", "g",
    "e", "h",
    "f", "k",
    "i", "j",
    "j", "k")

  val afterBypassAllTaxonomy = taxonomyFromEdgeList(afterBypassAllEdgeSpec)

  // After bypass(i, {c, e})

  val afterReduceEdgeSpec = List("a", "b",
    "a", "c",
    "b", "d",
    "b", "e",
    "b", "i",
    "c", "f",
    "c", "g",
    "e", "h",
    "f", "k",
    "i", "j",
    "j", "k")

  val afterReduceTaxonomy = taxonomyFromEdgeList(afterReduceEdgeSpec)

  val afterIsolateOneEdgeSpec = List(
    "a", "b",
    "a", "c\\i",
    "b", "d",
    "b", "e",
    "b", "i",
    "c\\i", "f",
    "c\\i", "g",
    "e", "h",
    "f", "k",
    "i", "j",
    "j", "k")

  val afterIsolateOneTaxonomy = taxonomyFromEdgeList(afterIsolateOneEdgeSpec)

  val afterIsolateAllEdgeSpec = List(
    "a", "b",
    "a", "c\\i",
    "b", "d",
    "b", "e\\i",
    "b", "i",
    "c\\i", "f",
    "c\\i", "g",
    "e\\i", "h",
    "f", "k",
    "i", "j",
    "j", "k")

  val afterIsolateAllTaxonomy = taxonomyFromEdgeList(afterIsolateAllEdgeSpec)

  val afterTreeifyEdgeSpec = List(
    "a", "b",
    "a", "c\\(i∪k)",
    "b", "d",
    "b", "e\\i",
    "b", "i\\k",
    "b", "k",
    "c\\(i∪k)", "f\\k",
    "c\\(i∪k)", "g",
    "e\\i", "h",
    "i\\k", "j\\k")

  val afterTreeifyTaxonomy = taxonomyFromEdgeList(afterTreeifyEdgeSpec)

  val siblingMap: SortedMap[ClassExpression[String], SortedSet[ClassExpression[String]]] =
    SortedMap.empty[ClassExpression[String], SortedSet[ClassExpression[String]]] +
    (string2ClassExpression("a") -> (SortedSet.empty ++ List("b", "c\\(i∪k)").map(string2ClassExpression))) +
    (string2ClassExpression("b") -> (SortedSet.empty ++ List("d", "e\\i", "i\\k", "k").map(string2ClassExpression))) +
    (string2ClassExpression("c\\(i∪k)") -> (SortedSet.empty ++ List("f\\k", "g").map(string2ClassExpression)))

  val disjointClassesAxioms = SortedSet.empty[ClassExpression[String]] ++
    siblingMap.values.map(DisjointClassesAxiom.apply)

  val disjointUnionAxioms = SortedSet.empty[ClassExpression[String]] ++
    siblingMap.map {
      case (Singleton(s), cs) =>
        DisjointUnionAxiom(SortedSet.empty ++ cs + Singleton(s))
      case (_, cs: SortedSet[ClassExpression[String]]) =>
        DisjointClassesAxiom(cs)
    }

  @Test def testChildrenOf(): Unit =
    val bc = SortedSet.empty ++ LazyList("b", "c").map(vertexMap.apply)
    assertEquals(bc, initialTaxonomy.childrenOf(vertexMap("a")))

  @Test def testDescendantsOf(): Unit =
    val bcdefghijk = SortedSet.empty ++ LazyList("b", "c", "d", "e", "f", "g", "h", "i", "j", "k").map(vertexMap.apply)
    assertEquals(bcdefghijk, initialTaxonomy.descendantsOf(vertexMap("a")))

  @Test def testDirectChildrenOf(): Unit =
    val bc = SortedSet.empty ++ LazyList("b", "c").map(vertexMap.apply)
    assertEquals(bc, initialTaxonomy.directChildrenOf(vertexMap("a")))

  @Test def testParentsOf(): Unit =
    val ce = SortedSet.empty ++ LazyList("c", "e").map(vertexMap.apply)
    assertEquals(ce, initialTaxonomy.parentsOf(vertexMap("i")))

  @Test def testAncestorsOf(): Unit =
    val abce = SortedSet.empty ++ LazyList("a", "b", "c", "e").map(vertexMap.apply)
    assertEquals(abce, initialTaxonomy.ancestorsOf(vertexMap("i")))

  @Test def testDirectParentsOf(): Unit =
    val ce = SortedSet.empty ++ LazyList("c", "e").map(vertexMap.apply)
    assertEquals(ce, initialTaxonomy.directParentsOf(vertexMap("i")))

  @Test def testMultiParentChild(): Unit =
    val childOption = initialTaxonomy.multiParentChild()
    assertTrue(childOption.isDefined)
    assertEquals(vertexMap("i"), childOption.get)

  @Test def testExciseVertex(): Unit =
    assertEquals(afterExciseVertexTaxonomy, initialTaxonomy.exciseVertex(vertexMap("i")))

  @Test def testExciseVertices(): Unit =
    val exciseSet = SortedSet.empty ++ LazyList("a", "b", "d", "e", "g", "h").map(vertexMap.apply)
    assertEquals(afterExciseVerticesTaxonomy, initialTaxonomy.exciseVertices(exciseSet))

  @Test def testExciseVerticesIf(): Unit =
    val exciseSet = SortedSet.empty ++ LazyList("a", "b", "d", "e", "g", "h").map(vertexMap.apply)
    assertEquals(afterExciseVerticesTaxonomy, initialTaxonomy.exciseVerticesIf(exciseSet.contains))

  @Test def testRootAt(): Unit =
    assertEquals(initialTaxonomy, unrootedTaxonomy.rootAt(vertexMap("a")))

  @Test def testTransitiveReduction(): Unit =
    assertEquals(initialTaxonomy, redundantEdgeTaxonomy.transitiveReduction())

  @Test def testBypassParent(): Unit =
    val c = vertexMap("c")
    val i = vertexMap("i")
    assertEquals(afterBypassOneTaxonomy, initialTaxonomy.bypassParent(i, c))

  @Test def testBypassParents(): Unit =
    val i = vertexMap("i")
    assertEquals(afterBypassAllTaxonomy, initialTaxonomy.bypassParents(i, initialTaxonomy.parentsOf(i)))

  @Test def testReduceChild(): Unit =
    val i = vertexMap("i")
    assertEquals(afterReduceTaxonomy, afterBypassAllTaxonomy.reduceChild(i))

  @Test def testIsolateChildFromOne(): Unit =
    val c = vertexMap("c")
    val i = vertexMap("i")
    assertEquals(afterIsolateOneTaxonomy, afterReduceTaxonomy.isolateChildFromOne(i, c))

  @Test def testIsolateChild(): Unit =
    val i = vertexMap("i")
    assertEquals(afterIsolateAllTaxonomy, afterReduceTaxonomy.isolateChild(i, initialTaxonomy.parentsOf(i)))

  @Test def testTreeify(): Unit =
    assertEquals(afterTreeifyTaxonomy, initialTaxonomy.treeify())

  @Test def testSiblingMap(): Unit =
    assertEquals(siblingMap, afterTreeifyTaxonomy.siblingMap())

  @Test def testGenerateClosureAxioms(): Unit =
    assertEquals(disjointClassesAxioms, initialTaxonomy.generateClosureAxioms(Axiom.DISJOINT_CLASSES))
    assertEquals(disjointUnionAxioms, initialTaxonomy.generateClosureAxioms(Axiom.DISJOINT_UNION))
