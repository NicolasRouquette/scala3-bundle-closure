import io.opencaesar.graph.*
import org.junit.{Assert, Before, Test}

import scala.collection.immutable.SortedSet

class TestAxiom2:

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

  @Test
  def test_djca1a(): Unit =
    val djca1a = DisjointClassesAxiom[String](SortedSet.empty[ClassExpression[String]] + aub + ciaub)
    Assert.assertEquals("djca1a", "DisjointClasses(" + djca1a.set.toSeq.map(_.toString).mkString(", ") + ")", djca1a.toString)

  @Test
  def test_djca1b(): Unit =
    val djca1b = DisjointClassesAxiom[String](SortedSet.empty[ClassExpression[String]] + bua + buaic)
    Assert.assertEquals("djca1b", "DisjointClasses(" + djca1b.set.toSeq.map(_.toString).mkString(", ") + ")", djca1b.toString)

  @Test
  def test_djca2a(): Unit =
    val djca2a = DisjointClassesAxiom[String](SortedSet.empty[ClassExpression[String]] + aub + ciaub + ciaubmd + e)
    Assert.assertEquals("djca2a", "DisjointClasses(" + djca2a.set.toSeq.map(_.toString).mkString(", ") +  ")", djca2a.toString)

  @Test
  def test_djca2b(): Unit =
    val djca2b = DisjointClassesAxiom[String](SortedSet.empty[ClassExpression[String]] + bua + buaic + buaicmd + e)
    Assert.assertEquals("djca2b", "DisjointClasses(" + djca2b.set.toSeq.map(_.toString).mkString(", ") +  ")", djca2b.toString)

  @Test
  def test_eqca1a(): Unit =
    val eqca1a = EquivalentClassesAxiom[String](SortedSet.empty[ClassExpression[String]] + aub + ciaub)
    Assert.assertEquals("eqca1a", "EquivalentClasses(" + eqca1a.set.toSeq.map(_.toString).mkString(", ") + ")", eqca1a.toString)

  @Test
  def test_eqca1b(): Unit =
    val eqca1b = EquivalentClassesAxiom[String](SortedSet.empty[ClassExpression[String]] + bua + buaic)
    Assert.assertEquals("eqca1b", "EquivalentClasses(" + eqca1b.set.toSeq.map(_.toString).mkString(", ") + ")", eqca1b.toString)

  @Test
  def test_eqca2a(): Unit =
    val eqca2a = EquivalentClassesAxiom[String](SortedSet.empty[ClassExpression[String]] + aub + ciaub + ciaubmd + e)
    Assert.assertEquals("eqca2a", "EquivalentClasses(" + eqca2a.set.toSeq.map(_.toString).mkString(", ") + ")", eqca2a.toString)

  @Test
  def test_eqca2b(): Unit =
    val eqca2b = EquivalentClassesAxiom[String](SortedSet.empty[ClassExpression[String]] + bua + buaic + buaicmd + e)
    Assert.assertEquals("eqca2b", "EquivalentClasses(" + eqca2b.set.toSeq.map(_.toString).mkString(", ") + ")", eqca2b.toString)

  @Test
  def test_djua1a(): Unit =
    val djua1a = DisjointUnionAxiom[String](SortedSet.empty[ClassExpression[String]] + aub + ciaub)
    Assert.assertEquals("djua1a", "DisjointUnion(" + djua1a.set.toSeq.map(_.toString).mkString(", ") + ")", djua1a.toString)

  @Test
  def test_djua1b(): Unit =
    val djua1b = DisjointUnionAxiom[String](SortedSet.empty[ClassExpression[String]] + bua + buaic)
    Assert.assertEquals("djua1b", "DisjointUnion(" + djua1b.set.toSeq.map(_.toString).mkString(", ") + ")", djua1b.toString)

  @Test
  def test_djua2a(): Unit =
    val djua2a = DisjointUnionAxiom[String](SortedSet.empty[ClassExpression[String]] + aub + ciaub + ciaubmd + e)
    Assert.assertEquals("djua2a", "DisjointUnion(" + djua2a.set.toSeq.map(_.toString).mkString(", ") + ")", djua2a.toString)

  @Test
  def test_djua2b(): Unit =
    val djua2b = DisjointUnionAxiom[String](SortedSet.empty[ClassExpression[String]] + bua + buaic + buaicmd + e)
    Assert.assertEquals("djua2b", "DisjointUnion(" + djua2b.set.toSeq.map(_.toString).mkString(", ") + ")", djua2b.toString)