package aoc2022

object Day1Part1 extends App {
  val sc = scannerFromResource("/day1_2021.txt")
  val lines = scannerToLines(sc).map(_.toInt)
  val increases = lines.sliding(2, 1).filter { ar => ar(0) < ar(1) }
  println("Solution: " + increases.size)
}

object Day1Part2 extends App {
  val sc = scannerFromResource("/day1_2021.txt")
  val lines = scannerToLines(sc).map(_.toInt)
  val sums = lines.sliding(3, 1).map { ar => ar(0) + ar(1) + ar(2) }
  val increases = sums.sliding(2, 1).filter { ar => ar(0) < ar(1) }
  println("Solution: " + increases.size)
}
