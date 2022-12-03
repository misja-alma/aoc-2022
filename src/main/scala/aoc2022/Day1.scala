package aoc2022


object Day1Part12 extends App {
  val sc = scannerFromResource("/day1.txt")
  val lines = scannerToLines(sc)

  val startState = (Seq[Seq[Int]](), Seq[Int]())
  val (food, _) = lines.foldLeft(startState) { (state, line) =>
      val (allFoods, elfFood) = state
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
