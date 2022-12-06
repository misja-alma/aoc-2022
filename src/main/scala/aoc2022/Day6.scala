package aoc2022

val sc = scannerFromResource("/day6.txt")
val lines = scannerToLines(sc)
val line = lines.head

def findMarkerIndex(length: Int): Option[Int] = {
  val market = line.sliding(length, 1).find { cs =>
    cs.toSet.size == length
  }
  market.map(line.indexOf(_) + length)
}

object Day6Part1 extends App {
  val solution = findMarkerIndex(4)
  println(solution)
}

object Day6Part2 extends App {
  val solution = findMarkerIndex(14)
  println(solution)
}
