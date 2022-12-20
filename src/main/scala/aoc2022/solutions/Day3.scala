package aoc2022.solutions

import aoc2022.utils._

object Day3 {
  def charScore(c: Char): Int =
    if c > 'Z' then c - 'a' + 1 else c - 'A' + 1 + 26

  @main
  def day3Part1 = printSolution {
    val sc = scannerFromResource("/day3.txt")
    val lines = scannerToLines(sc)

    val sharedChars = lines.map { line =>
      val trimmed = line.trim
      val halfLength = trimmed.length / 2
      val half1 = trimmed.take(halfLength).toSet
      val half2 = trimmed.drop(halfLength).toSet
      val shared = half1.intersect(half2)
      shared.head
    }

    val solutions = sharedChars.map(charScore)
    solutions.sum
  }

  @main
  def day3Part2 = printSolution {
    val sc = scannerFromResource("/day3.txt")
    val lines = scannerToLines(sc)

    val sharedChars = lines.sliding(3, 3).map { case p: Seq[String] =>
      val shared = p.map(_.toSet).reduce(_ intersect _)
      shared.head
    }

    val solutions = sharedChars.map(charScore)
    solutions.sum
  }
}
