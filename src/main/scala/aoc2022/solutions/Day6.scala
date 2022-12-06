package aoc2022.solutions

import aoc2022.utils._

object Day6 {
  val sc = scannerFromResource("/day6.txt")
  val lines = scannerToLines(sc)
  val line = lines.head

  def findMarkerIndex(length: Int): Option[Int] = {
    val market = line.sliding(length, 1).find { cs =>
      cs.toSet.size == length
    }
    market.map(line.indexOf(_) + length)
  }

  @main
  def day6Part1 = {
    val solution = findMarkerIndex(4)
    println(solution)
  }

  @main
  def day6Part2 = {
    val solution = findMarkerIndex(14)
    println(solution)
  }
}
