import io.opencaesar.graph.*

import scala.collection.immutable.SortedSet
import org.junit.Assert
import org.junit.{Before,Test} 

class TestAxiom:

  val a = ClassExpression.Singleton[String]("a")
  val b = ClassExpression.Singleton[String]("b")
  val c = ClassExpression.Singleton[String]("c")
  val d = ClassExpression.Singleton[String]("d")
  val e = ClassExpression.Singleton[String]("e")

  val aub = ClassExpression.Union[String](SortedSet.empty[ClassExpression[String]] + a + b)
  val bua = ClassExpression.Union[String](SortedSet.empty[ClassExpression[String]] + b + a)

  val ciaub = ClassExpression.Intersection[String](SortedSet.empty[ClassExpression[String]] + c + aub)
  val buaic = ClassExpression.Intersection[String](SortedSet.empty[ClassExpression[String]] + bua + c)

  val ciaubmd = ClassExpression.Difference[String](ciaub, d)
  val buaicmd = ClassExpression.Difference[String](buaic, d)

  val djca1a = DisjointClassesAxiom[String](SortedSet.empty[ClassExpression[String]] + aub + ciaub)
  val djca1b = DisjointClassesAxiom[String](SortedSet.empty[ClassExpression[String]] + bua + buaic)
  val djca2a = DisjointClassesAxiom[String](SortedSet.empty[ClassExpression[String]] + aub + ciaub + ciaubmd + e)
  val djca2b = DisjointClassesAxiom[String](SortedSet.empty[ClassExpression[String]] + bua + buaic + buaicmd + e)

  val eqca1a = EquivalentClassesAxiom[String](SortedSet.empty[ClassExpression[String]] + aub + ciaub)
  val eqca1b = EquivalentClassesAxiom[String](SortedSet.empty[ClassExpression[String]] + bua + buaic)
  val eqca2a = EquivalentClassesAxiom[String](SortedSet.empty[ClassExpression[String]] + aub + ciaub + ciaubmd + e)
  val eqca2b = EquivalentClassesAxiom[String](SortedSet.empty[ClassExpression[String]] + bua + buaic + buaicmd + e)
  
  val djua1a = DisjointUnionAxiom[String](e, SortedSet.empty[ClassExpression[String]] + aub + ciaub)
  val djua1b = DisjointUnionAxiom[String](e, SortedSet.empty[ClassExpression[String]] + bua + buaic)
  val djua2a = DisjointUnionAxiom[String](e, SortedSet.empty[ClassExpression[String]] + aub + ciaub + ciaubmd + e)
  val djua2b = DisjointUnionAxiom[String](e, SortedSet.empty[ClassExpression[String]] + bua + buaic + buaicmd + e)

  @Test
  def testHashCode(): Unit =

    Assert.assertEquals(djca1a.hashCode(), djca1b.hashCode())
    Assert.assertEquals(djca2a.hashCode(), djca2b.hashCode())

    Assert.assertEquals(eqca1a.hashCode(), eqca1b.hashCode())
    Assert.assertEquals(eqca2a.hashCode(), eqca2b.hashCode())

    Assert.assertEquals(djua1a.hashCode(), djua1b.hashCode())
    Assert.assertEquals(djua2a.hashCode(), djua2b.hashCode())

  @Test
  def testEquals(): Unit = 
    // equivalent class expressions
    Assert.assertEquals(djca1a, djca1b)
    Assert.assertEquals(djca2a, djca2b)
    
    // non-equivalent class expressions
    Assert.assertNotEquals(djca1a, djca2a)
    Assert.assertNotEquals(djca1a, djca2b)
    Assert.assertNotEquals(djca1b, djca2a)
    Assert.assertNotEquals(djca1b, djca2b)

    // equivalent class expressions
    Assert.assertEquals(eqca1a, eqca1b)
    Assert.assertEquals(eqca2a, eqca2b)

    // non-equivalent class expressions
    Assert.assertNotEquals(eqca1a, eqca2a)
    Assert.assertNotEquals(eqca1a, eqca2b)
    Assert.assertNotEquals(eqca1b, eqca2a)
    Assert.assertNotEquals(eqca1b, djca2b)

    // equivalent class expressions
    Assert.assertEquals(djua1a, djua1b)
    Assert.assertEquals(djua2a, djua2b)

    // non-equivalent class expressions
    Assert.assertNotEquals(djua1a, djua2a)
    Assert.assertNotEquals(djua1a, djua2b)
    Assert.assertNotEquals(djua1b, djua2a)
    Assert.assertNotEquals(djua1b, djca2b)

    // different axiom types
    Assert.assertNotEquals(djca1a, eqca1a)
    Assert.assertNotEquals(djca1a, eqca1b)
    Assert.assertNotEquals(djca1a, eqca2a)
    Assert.assertNotEquals(djca1a, eqca2b)

    Assert.assertNotEquals(djca1b, eqca1a)
    Assert.assertNotEquals(djca1b, eqca1b)
    Assert.assertNotEquals(djca1b, eqca2a)
    Assert.assertNotEquals(djca1b, eqca2b)

    Assert.assertNotEquals(djca2a, eqca1a)
    Assert.assertNotEquals(djca2a, eqca1b)
    Assert.assertNotEquals(djca2a, eqca2a)
    Assert.assertNotEquals(djca2a, eqca2b)

    Assert.assertNotEquals(djca2b, eqca1a)
    Assert.assertNotEquals(djca2b, eqca1b)
    Assert.assertNotEquals(djca2b, eqca2a)
    Assert.assertNotEquals(djca2b, eqca2b)

    Assert.assertNotEquals(djca1a, djua1a)
    Assert.assertNotEquals(djca1a, djua1b)
    Assert.assertNotEquals(djca1a, djua2a)
    Assert.assertNotEquals(djca1a, djua2b)

    Assert.assertNotEquals(djca1b, djua1a)
    Assert.assertNotEquals(djca1b, djua1b)
    Assert.assertNotEquals(djca1b, djua2a)
    Assert.assertNotEquals(djca1b, djua2b)

    Assert.assertNotEquals(djca2a, djua1a)
    Assert.assertNotEquals(djca2a, djua1b)
    Assert.assertNotEquals(djca2a, djua2a)
    Assert.assertNotEquals(djca2a, djua2b)

    Assert.assertNotEquals(djca2b, djua1a)
    Assert.assertNotEquals(djca2b, djua1b)
    Assert.assertNotEquals(djca2b, djua2a)
    Assert.assertNotEquals(djca2b, djua2b)

    Assert.assertNotEquals(eqca1a, djua1a)
    Assert.assertNotEquals(eqca1a, djua1b)
    Assert.assertNotEquals(eqca1a, djua2a)
    Assert.assertNotEquals(eqca1a, djua2b)

    Assert.assertNotEquals(eqca1b, djua1a)
    Assert.assertNotEquals(eqca1b, djua1b)
    Assert.assertNotEquals(eqca1b, djua2a)
    Assert.assertNotEquals(eqca1b, djua2b)

    Assert.assertNotEquals(eqca2a, djua1a)
    Assert.assertNotEquals(eqca2a, djua1b)
    Assert.assertNotEquals(eqca2a, djua2a)
    Assert.assertNotEquals(eqca2a, djua2b)

    Assert.assertNotEquals(eqca2b, djua1a)
    Assert.assertNotEquals(eqca2b, djua1b)
    Assert.assertNotEquals(eqca2b, djua2a)
    Assert.assertNotEquals(eqca2b, djua2b)

  @Test
  def testToString(): Unit =
    // TODO: Why is .toSeq necessary?
    
    Assert.assertEquals("djca1a", "DisjointClasses(" + djca1a.set.toSeq.map(_.toString()).mkString(", ") + ")", djca1a.toString())
    Assert.assertEquals("djca1b", "DisjointClasses(" + djca1b.set.toSeq.map(_.toString()).mkString(", ") + ")", djca1b.toString())
    Assert.assertEquals("djca2a", "DisjointClasses(" + djca2a.set.toSeq.map(_.toString()).mkString(", ") +  ")", djca2a.toString())
    Assert.assertEquals("djca2b", "DisjointClasses(" + djca2b.set.toSeq.map(_.toString()).mkString(", ") +  ")", djca2b.toString())

    Assert.assertEquals("eqca1a", "EquivalentClasses(" + eqca1a.set.toSeq.map(_.toString()).mkString(", ") + ")", eqca1a.toString())
    Assert.assertEquals("eqca1b", "EquivalentClasses(" + eqca1b.set.toSeq.map(_.toString()).mkString(", ") + ")", eqca1b.toString())
    Assert.assertEquals("eqca2a", "EquivalentClasses(" + eqca2a.set.toSeq.map(_.toString()).mkString(", ") + ")", eqca2a.toString())
    Assert.assertEquals("eqca2b", "EquivalentClasses(" + eqca2b.set.toSeq.map(_.toString()).mkString(", ") + ")", eqca2b.toString())

    Assert.assertEquals("djua1a", "DisjointUnion(" + djua1a.c + "," + djua1a.set.toSeq.map(_.toString()).mkString(", ") + ")", djua1a.toString())
    Assert.assertEquals("djua1b", "DisjointUnion(" + djua1a.c + "," + djua1b.set.toSeq.map(_.toString()).mkString(", ") + ")", djua1b.toString())
    Assert.assertEquals("djua2a", "DisjointUnion(" + djua1a.c + "," + djua2a.set.toSeq.map(_.toString()).mkString(", ") + ")", djua2a.toString())
    Assert.assertEquals("djua2b", "DisjointUnion(" + djua1a.c + "," + djua2b.set.toSeq.map(_.toString()).mkString(", ") + ")", djua2b.toString())