package aoc2022.solutions

import aoc2022.utils.*

object Day20 {
  val sc = scannerFromResource("/day20.txt")
  val lines = scannerToLines(sc)


  def shiftAll(original: List[Long], shifted: scala.collection.mutable.ListBuffer[(Long, Int)]) = {
    original.zipWithIndex.foreach { case org@(nr, _) =>
      val shiftPairIndex = shifted.indexOf(org)

      val normalizedShift = nr % (original.length - 1) // note: we rotate over all elements except ourselves
      val add1IfPositive = if nr > 0 then 1 else 0
      val newIndexRaw = shiftPairIndex + normalizedShift + add1IfPositive
      val newIndex = (newIndexRaw + 2 * original.length) % original.length
      val insertAtElement = shifted(newIndex.toInt)
      if insertAtElement != org then {
        shifted.remove(shiftPairIndex)
        val indexAfterRemove = shifted.indexOf(insertAtElement)
        shifted.insert(indexAfterRemove, org)
      }
    }
  }
  

  @main
  def day20Part1 = printSolution {   // 6387
    val original = lines.map(_.toLong).toList
    val shifted = scala.collection.mutable.ListBuffer.from(original.zipWithIndex)

    shiftAll(original, shifted)

    val indexOf0 = shifted.map(_._1).indexOf(0)
    val elements = Seq(1000, 2000, 3000).map {  i =>
      val iFind = (indexOf0 + i) % original.length
      shifted(iFind)._1
    }
    
    elements.sum
  }

  @main
  def day20Part2 = printSolution {
    val original = lines.map(_.toInt).toList.map { _ * 811589153L }
    val shifted = scala.collection.mutable.ListBuffer.from(original.zipWithIndex)

    (1 to 10).foreach { i =>
      println ("Shift " + i)
      shiftAll(original, shifted)
    }

    val indexOf0 = shifted.map(_._1).indexOf(0)
    val elements = Seq(1000, 2000, 3000).map { i =>
      val iFind = (indexOf0 + i) % original.length
      shifted(iFind)._1
    }

    elements.sum
  }
}
