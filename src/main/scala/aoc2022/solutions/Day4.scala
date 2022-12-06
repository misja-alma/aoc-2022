package aoc2022.solutions

import aoc2022.utils._

object Day4Part1 extends App {
  val sc = scannerFromResource("/day4.txt")
  val lines = scannerToLines(sc)

  val solution = lines.count { line =>
    val (int1, int2) = line match {
      case s"$min1-$max1,$min2-$max2" => Interval(min1.toInt, max1.toInt) -> Interval(min2.toInt, max2.toInt)
    }
    
    int1.contains(int2) || int2.contains(int1)
  }

  println (solution)
}

object Day4Part2 extends App {
  val sc = scannerFromResource("/day4.txt")
  val lines = scannerToLines(sc)

  val solution = lines.count { line =>
    val (int1, int2) = line match {
      case s"$min1-$max1,$min2-$max2" => Interval(min1.toInt, max1.toInt) -> Interval(min2.toInt, max2.toInt)
    }

    int1.intersect(int2).nonEmpty
  }
  
  println(solution)
}
