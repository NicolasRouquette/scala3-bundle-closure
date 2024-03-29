import io.opencaesar.graph.*
import io.opencaesar.graph.ClassExpression.*

import scala.collection.immutable.SortedSet
import org.junit.Assert.*
import org.junit.{Before,Test} 

class TestDifference:

  val sa1 = Singleton("a")
  val sa2 = Singleton("a")
  val sb = Singleton("b")
  val sc = Singleton("c")

  val a1ma2 = Difference(sa1, sa2)
  val a2ma1 = Difference(sa2, sa1)
  val a1mb = Difference(sa1, sb)
  val a2mb = Difference(sa2, sb)
  val bma1 = Difference(sb, sa1)
  val bmb = Difference(sb, sb)
  val bmc = Difference(sb, sc)
  val empty = Empty[String]()
  val universal = Universal[String]()

  @Test
  def testHashCode(): Unit =
    assertEquals(a1ma2.hashCode(), a2ma1.hashCode())
    assertNotEquals(a1ma2.hashCode(), a1mb.hashCode())
    assertNotEquals(a1ma2.hashCode(), a2mb.hashCode())
    assertNotEquals(a1ma2.hashCode(), bma1.hashCode())
    assertNotEquals(a1ma2.hashCode(), bmb.hashCode())
    assertEquals(a2ma1.hashCode(), a1ma2.hashCode())
    assertNotEquals(a2ma1.hashCode(), a1mb.hashCode())
    assertNotEquals(a2ma1.hashCode(), a2mb.hashCode())
    assertNotEquals(a2ma1.hashCode(), bma1.hashCode())
    assertNotEquals(a2ma1.hashCode(), bmb.hashCode())
    assertNotEquals(a1mb.hashCode(), a1ma2.hashCode())
    assertNotEquals(a1mb.hashCode(), a2ma1.hashCode())
    assertEquals(a1mb.hashCode(), a2mb.hashCode())
    assertNotEquals(a1mb.hashCode(), bma1.hashCode())
    assertNotEquals(a1mb.hashCode(), bmb.hashCode())
    assertNotEquals(a2mb.hashCode(), a1ma2.hashCode())
    assertNotEquals(a2mb.hashCode(), a2ma1.hashCode())
    assertEquals(a2mb.hashCode(), a1mb.hashCode())
    assertNotEquals(a2mb.hashCode(), bma1.hashCode())
    assertNotEquals(a2mb.hashCode(), bmb.hashCode())
    assertNotEquals(bma1.hashCode(), a1ma2.hashCode())
    assertNotEquals(bma1.hashCode(), a2ma1.hashCode())
    assertNotEquals(bma1.hashCode(), a1mb.hashCode())
    assertNotEquals(bma1.hashCode(), a2mb.hashCode())
    assertNotEquals(bma1.hashCode(), bmb.hashCode())
    assertNotEquals(bmb.hashCode(), a1ma2.hashCode())
    assertNotEquals(bmb.hashCode(), a2ma1.hashCode())
    assertNotEquals(bmb.hashCode(), a1mb.hashCode())
    assertNotEquals(bmb.hashCode(), a2mb.hashCode())
    assertNotEquals(bmb.hashCode(), bma1.hashCode())
    assertNotEquals(bmb.hashCode(), "dummy".hashCode())
    
  @Test
  def testDifference(): Unit =
    assertNotNull(a1ma2)
    assertNotNull(a2ma1)
    assertNotNull(a1mb)
    assertNotNull(a2mb)
    assertNotNull(bma1)
    assertNotNull(bmb)

  @Test
  def testDifference1(): Unit =
    assertEquals(a1mb, sa1.difference(sb));
    assertEquals(a2mb, sa2.difference(sb));
    assertEquals(bma1, sb.difference(sa1));
    // Theorem 8
    val d = Difference(sa1, Union(SortedSet.empty[ClassExpression[String]] + bma1 + bmc))
    assertEquals(d, sa1.difference(bma1).difference(bmc))
    assertEquals(d, sa1.difference(bmc).difference(bma1))
    // Theorem 11
    assertEquals(a1mb, a1mb.difference(empty))
    // Theorem 13
    assertEquals(empty, a1mb.difference(a1mb))
    // Theorem 16
    assertEquals(empty, a1mb.difference(universal))

  @Test
  def testEqualsObject(): Unit =
    assertEquals(a1ma2, a2ma1)
    assertNotEquals(a1ma2, a1mb)
    assertNotEquals(a1ma2, a2mb)
    assertNotEquals(a1ma2, bma1)
    assertNotEquals(a1ma2, bmb)
    assertEquals(a2ma1, a1ma2)
    assertNotEquals(a2ma1, a1mb)
    assertNotEquals(a2ma1, a2mb)
    assertNotEquals(a2ma1, bma1)
    assertNotEquals(a2ma1, bmb)
    assertNotEquals(a1mb, a1ma2)
    assertNotEquals(a1mb, a2ma1)
    assertEquals(a1mb, a2mb)
    assertNotEquals(a1mb, bma1)
    assertNotEquals(a1mb, bmb)
    assertNotEquals(a2mb, a1ma2)
    assertNotEquals(a2mb, a2ma1)
    assertEquals(a2mb, a1mb)
    assertNotEquals(a2mb, bma1)
    assertNotEquals(a2mb, bmb)
    assertNotEquals(bma1, a1ma2)
    assertNotEquals(bma1, a2ma1)
    assertNotEquals(bma1, a1mb)
    assertNotEquals(bma1, a2mb)
    assertNotEquals(bma1, bmb)
    assertNotEquals(bmb, a1ma2)
    assertNotEquals(bmb, a2ma1)
    assertNotEquals(bmb, a1mb)
    assertNotEquals(bmb, a2mb)
    assertNotEquals(bmb, bma1)
    assertNotEquals(bmb, 1)

  @Test
  def testToString(): Unit =
    assertEquals("a\\a", a1ma2.toString)
    assertEquals("a\\a", a2ma1.toString)
    assertEquals("a\\b", a1mb.toString)
    assertEquals("a\\b", a2mb.toString)
    assertEquals("b\\a", bma1.toString)
    assertEquals("b\\b", bmb.toString)

  @Test
  def testComplement(): Unit =
    val a1ma2c = Complement(a1ma2)
    val a1mbc = Complement(a1mb)
    assertEquals(a1ma2c, a1ma2.complement())
    assertEquals(a1ma2c, a2ma1.complement())
    assertEquals(a1mbc, a1mb.complement())
    assertEquals(a1mbc, a2mb.complement())
    // Theorem 1
    assertEquals(a1mb, a1mb.complement().complement())

  @Test
  def testIntersection(): Unit =
    val i = Intersection(SortedSet.empty[ClassExpression[String]] + a1ma2 + a1mb)
    assertEquals(i, a1ma2.intersection(a1mb))
    assertEquals(i, a1mb.intersection(a1ma2))
    // Theorem 2
    assertEquals(a1mb, a1mb.intersection(a1mb))
    // Theorem 3
    assertEquals(a1mb.intersection(bmc), bmc.intersection(a1mb))
    // Theorem 4
    assertEquals((a1mb.intersection(bmb)).intersection(bmc), (a1mb.intersection(bmb)).intersection(bmc))

  @Test
  def testUnion(): Unit =
    val u = Union(SortedSet.empty[ClassExpression[String]] + a1ma2 + a1mb)
    assertEquals(u, a1ma2.union(a1mb))
    assertEquals(u, a1mb.union(a1ma2))
    // Theorem 5
    assertEquals(a1mb, a1mb.union(a1mb))
    // Theorem 6
    assertEquals(a1mb.union(bmc), bmc.union(a1mb))
    // Theorem 7
    assertEquals((a1mb.union(bmb)).union(bmc), (a1mb.union(bmb)).union(bmc))

  @Test 
  def testToAtom(): Unit =
    assertEquals("(a\\a)", a1ma2.toAtom)
    assertEquals("(a\\a)", a2ma1.toAtom)
    assertEquals("(a\\b)", a1mb.toAtom)
    assertEquals("(a\\b)", a2mb.toAtom)
    assertEquals("(b\\a)", bma1.toAtom)
    assertEquals("(b\\b)", bmb.toAtom)