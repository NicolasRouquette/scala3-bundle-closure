import io.opencaesar.graph.*
import io.opencaesar.graph.ClassExpression.*

import scala.collection.immutable.SortedSet
import org.junit.Assert.*
import org.junit.{Before,Test} 

object TestComplement:

  val sa1 = Singleton("a")
  val sa2 = Singleton("a")
  val sb = Singleton("b")
  val sc = Singleton("c")

  val ca1 = Complement(sa1)
  val ca2 = Complement(sa2)
  val cb = Complement(sb)
  val cc = Complement(sc)

  val empty = Empty[String]()
  val universal = Universal[String]()

  @Test
  def testHashCode(): Unit =
    assertEquals(ca1.hashCode, ca2.hashCode)
    assertNotEquals(ca1.hashCode, cb.hashCode)
    assertNotEquals(ca2.hashCode, cb.hashCode)

  @Test
  def testToAtom(): Unit =
    val caa = "a\u2032"
    val cba = "b\u2032"
    assertEquals(caa, ca1.toAtom())
    assertEquals(caa, ca2.toAtom())
    assertEquals(cba, cb.toAtom())

  @Test
  def testComplement1(): Unit =
    // Theorem 1
    assertEquals(sa1, ca1.complement())
    assertEquals(sa2, ca2.complement())
    assertEquals(sb, cb.complement())
    assertEquals(sa1, sa1.complement().complement())

  @Test
  def testEqualsObject(): Unit =
    assertEquals(ca1, ca2)
    assertNotEquals(ca1, cb)
    assertNotEquals(ca2, cb)

  @Test
  def testToString(): Unit =
    val caa = "a\u2032"
    val cba = "b\u2032"
    assertEquals(caa, ca1.toString())
    assertEquals(caa, ca2.toString())
    assertEquals(cba, cb.toString())

  @Test
  def testDifference(): Unit =
    val amb = Difference(ca1, cb)
    val bma = Difference(cb, ca1)
    assertEquals(amb, ca1.difference(cb))
    assertEquals(bma, cb.difference(ca1))
    // Theorem 8
    val u = Union(SortedSet.empty + cb + cc)
    assertEquals(ca1.difference(u), ca1.difference(cb).difference(cc))
    assertEquals(ca1.difference(u), ca1.difference(cc).difference(cb))
    // Theorem 11
    assertEquals(ca1, ca1.difference(empty))
    // Theorem 13
    assertEquals(empty, ca1.difference(ca1))
    // Theorem 16
    assertEquals(empty, ca1.difference(universal))

  @Test
  def testIntersection(): Unit =
    val i = Intersection(SortedSet.empty + ca1 + cb)
    assertEquals(i, ca1.intersection(cb))
    assertEquals(i, cb.intersection(ca1))
    // Theorem 2
    assertEquals(ca1, ca1.intersection(ca1))
    // Theorem 3
    assertEquals(ca1.intersection(cb), cb.intersection(ca1))
    // Theorem 4
    assertEquals((ca1.intersection(cb)).intersection(cc), (ca1.intersection(cb)).intersection(cc))

  @Test
  def testUnion(): Unit =
    val u = Union(SortedSet.empty + ca1 + cb)
    assertEquals(u, ca1.union(cb))
    assertEquals(u, cb.union(ca1))
    // Theorem 5
    assertEquals(ca1, ca1.union(ca1))
    // Theorem 6
    assertEquals(ca1.union(cb), cb.union(ca1))
    // Theorem 7
    assertEquals((ca1.union(cb)).union(cc), (ca1.union(cb)).union(cc)) // Theorem 15