package aoc2022.solutions

import aoc2022.utils.*


object Day10 {
  val sc = scannerFromResource("/day10.txt")
  val lines = scannerToLines(sc)

  val noop = """noop""".r
  val addx = """addx (-?\d+)""".r

  enum Output:
    case Noop() extends Output
    case AddX(add: Int) extends Output
    
  import Output._

  def parseOutput(s: String): Output = s.trim() match {
    case noop() => Noop()
    case addx(target) => AddX(target.toInt)
    case _ => sys.error("Cant parse: " + s)
  }

  val ops = lines.map(parseOutput)

  case class State(x: Int, cycleValues: List[Int])

  val State(finalX, finalValues) = ops.foldLeft(State(1, List())) { case (st, op) =>
    op match {
      case Noop() =>
        State(st.x, st.cycleValues :+ st.x)
      case AddX(n: Int) =>
        val newX = st.x + n
        State(newX, st.cycleValues ++ Seq(st.x, st.x))
    }
  }


  @main
  def day10Part1 = {
    val toCheck = Seq(20, 60, 100, 140, 180, 220)
    val solution = toCheck.map { nr =>
      nr * finalValues(nr - 1)
    }.sum

    println("Solution: " + solution)
  }

  @main
  def day10Part2 = {
    val points = finalValues.zipWithIndex.flatMap { case (x, index) =>
      val pointer = index % 40
      if (pointer >= x - 1 && pointer <= x + 1) {
        Some(Point(index % 40, index / 40))
      } else None
    }

    val grid = Grid.fromPoints(points)
    Grid.printBooleanGrid(grid)
  }
}
