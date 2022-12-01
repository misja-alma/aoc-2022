package aoc2022

import collection._

object Day1Part12 extends App {
  val sc = scannerFromResource("/day1.txt")
  val lines = scannerToLines(sc)

  val startState = (Seq[Seq[Int]](), Seq[Int]())
  val (food, _) = lines.foldLeft(startState) {
    case ((allFoods, elfFood), line) =>
      if line.trim.isEmpty then
        (allFoods :+ elfFood, Seq[Int]())
      else
        (allFoods, elfFood :+ line.toInt)
  }
  
  val solution = food.map(_.sum).max
  println(solution)

  val solution2 = food.map(_.sum).sorted.reverse.take(3).sum
  println(solution2)
}
