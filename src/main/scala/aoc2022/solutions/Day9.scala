package aoc2022.solutions

import aoc2022.utils.*

import scala.collection.mutable.ListBuffer

object Day9 {
  val sc = scannerFromResource("/day9.txt")
  val lines = scannerToLines(sc)

  // H, T start at 0,0


  def followH(hPos: Point, tPos: Point): Point = {
    // if difx and difx <= 1: do nothing
    // otherwise: the one with dif1 => dif 0
    // the one with dif2 => dif1
    val xDif = hPos.x - tPos.x
    val yDif = hPos.y - tPos.y
    if Math.abs(xDif) <= 1 && Math.abs(yDif) <= 1 then tPos
    else {
      (xDif, yDif) match {
        case (2, 0) => tPos.copy(x = tPos.x + 1)
        case (-2, 0) => tPos.copy(x = tPos.x - 1)

        case (2, 1) => tPos.copy(x = tPos.x + 1, y = hPos.y)
        case (-2, 1) => tPos.copy(x = tPos.x - 1, y = hPos.y)

        case (2, -1) => tPos.copy(x = tPos.x + 1, y = hPos.y)
        case (-2, -1) => tPos.copy(x = tPos.x - 1, y = hPos.y)

        case (0, 2) => tPos.copy(y = tPos.y + 1)
        case (0, -2) => tPos.copy(y = tPos.y - 1)

        case (1, 2) => tPos.copy(y = tPos.y + 1, x = hPos.x)
        case (1, -2) => tPos.copy(y = tPos.y - 1, x = hPos.x)

        case (-1, 2) => tPos.copy(y = tPos.y + 1, x = hPos.x)
        case (-1, -2) => tPos.copy(y = tPos.y - 1, x = hPos.x)

        // TODO not sure if needed
        case (2, 2) => tPos.copy(x = tPos.x + 1, y = tPos.y + 1)
        case (-2, -2) => tPos.copy(x = tPos.x - 1, y = tPos.y - 1)
        case (2, -2) => tPos.copy(x = tPos.x + 1, y = tPos.y - 1)
        case (-2, 2) => tPos.copy(x = tPos.x - 1, y = tPos.y + 1)

      }
    }
  }

  @main
  def day9Part1 = {

    case class State(hPos: Point, tPos: Point, visited: Set[Point])

    val finalState = lines.foldLeft(State(Point(0,0), Point(0,0), Set(Point(0,0)))){ case (state: State, line) =>
      line match {
        case s"L $s" =>
          (s.toInt until 0 by -1).foldLeft(state) { case (state: State, _) =>
            val newH = state.hPos.copy(x = state.hPos.x - 1)
            val newT = followH(newH, state.tPos)
            val newVisited = state.visited + newT
            State(newH, newT, newVisited)
          }
        case s"R $s" =>
          (s.toInt until 0 by -1).foldLeft(state) { case (state: State, _) =>
            val newH = state.hPos.copy(x = state.hPos.x + 1)
            val newT = followH(newH, state.tPos)
            val newVisited = state.visited + newT
            State(newH, newT, newVisited)
          }

        case s"U $s" =>
          (s.toInt until 0 by -1).foldLeft(state) { case (state: State, _) =>
            val newH = state.hPos.copy(y = state.hPos.y - 1)
            val newT = followH(newH, state.tPos)
            val newVisited = state.visited + newT
            State(newH, newT, newVisited)
          }

        case s"D $s" =>
          (s.toInt until 0 by -1).foldLeft(state) { case (state: State, _) =>
            val newH = state.hPos.copy(y = state.hPos.y + 1)
            val newT = followH(newH, state.tPos)
            val newVisited = state.visited + newT
            State(newH, newT, newVisited)
          }

        case _ => sys.error ("Can parse line: " + line)
      }
    }

    val solution = finalState.visited.size

    println ("Solution: " + solution)

  }

  @main
  def day9Part2 = {

    case class State(hPos: Point, tPos: List[Point], visited: Set[Point]) {
      def allPoints = hPos +: tPos
    }

    def newTrailers(first: Point, trailers: Seq[Point]): List[Point] = {
      var previous = first
      val result = ListBuffer[Point]()

      for pos <- trailers do {
        val newT = followH(previous, pos)
        previous = newT
        result.append(newT)
      }

      result.toList
    }

    val startTs = List.fill(9)(Point(0,0))
    val finalState = lines.foldLeft(State(Point(0, 0), startTs, Set(Point(0, 0)))) { case (state: State, line) =>

      line match {

        case s"L $s" =>
          (s.toInt until 0 by -1).foldLeft(state) { case (state: State, _) =>
            val newH = state.hPos.copy(x = state.hPos.x - 1)
            val newT = followH(newH, state.tPos.head)

            val newTail = newTrailers(newT, state.tPos.tail)
            val newVisited = state.visited + newTail.last

            State(newH, newT +: newTail, newVisited)
          }
        case s"R $s" =>
          (s.toInt until 0 by -1).foldLeft(state) { case (state: State, _) =>
            val newH = state.hPos.copy(x = state.hPos.x + 1)
            val newT = followH(newH, state.tPos.head)

            val newTail = newTrailers(newT, state.tPos.tail)

            val newVisited = state.visited + newTail.last
            State(newH, newT +: newTail, newVisited)
          }

        case s"U $s" =>
          (s.toInt until 0 by -1).foldLeft(state) { case (state: State, _) =>
            val newH = state.hPos.copy(y = state.hPos.y - 1)
            println ("NewH: " + newH)
            val newT = followH(newH, state.tPos.head)

            val newTail = newTrailers(newT, state.tPos.tail)

            val newVisited = state.visited + newTail.last
            State(newH, newT +: newTail, newVisited)
          }

        case s"D $s" =>
          (s.toInt until 0 by -1).foldLeft(state) { case (state: State, _) =>
            val newH = state.hPos.copy(y = state.hPos.y + 1)
            val newT = followH(newH, state.tPos.head)

            val newTail = newTrailers(newT, state.tPos.tail)

            val newVisited = state.visited + newTail.last
            State(newH, newT +: newTail, newVisited)
          }

        case _ => sys.error("Cant parse line: " + line)
      }
    }

    val solution = finalState.visited.size

    println("Solution: " + solution)
  }
}
