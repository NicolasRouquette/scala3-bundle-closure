
import org.junit.Test
import scala.collection.immutable.SortedMap

class SortedMapOrdering:

  val vs: Set[(String, Int)] =
    Set.empty +
      ("A" -> 3) +
      ("B" -> 2) +
      ("C" -> 1)

  val c1: SortedMap[String, Int] =
    SortedMap.empty ++ vs

  val vo: Ordering[String] = (x: String, y: String) =>
    c1.getOrElse(x, 0).compare(c1.getOrElse(y, 0))

  val c2: SortedMap[String, Int] =
    SortedMap.empty(vo) ++ vs

  @Test def test(): Unit =
    println(s"c1 = ${c1.mkString(", ")}")
    println(s"c2 = ${c2.mkString(", ")}")

