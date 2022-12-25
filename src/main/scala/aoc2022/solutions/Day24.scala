package aoc2022.solutions

import aoc2022.utils.{Point, *}
import aoc2022.utils.Search.Path

object Day24 {
  val sc = scannerFromResource("/day24.txt")
  val lines = scannerToLines(sc)

  val blizzards = lines.zipWithIndex.flatMap { case (line, row) =>
    line.trim.zipWithIndex.flatMap { case (c, col) => c match {
        case '>'|'<'|'^'|'v' => Some(c -> Point(col, row))
        case _ => None
      }
    }
  }
  val leftWall = 0
  val topWall = 0
  val bottomWall = 21
  val rightWall = 151
  // so x <- 1..20 and y <- 1..150 are the field
  val startPt = Point(1,0)
  val endPt = Point(150, 21)

  def validMove(pt: Point): Boolean = {
    (pt.x > leftWall && pt.x < rightWall && pt.y > topWall && pt.y < bottomWall) || pt == startPt || pt == endPt
  }

  def moveLeft(pt: Point): Point = {
    if pt.x > leftWall + 1 then pt.copy(x = pt.x - 1) else (pt.copy(x = rightWall -1))
  }

  def moveRight(pt: Point): Point = {
    if pt.x < rightWall - 1 then pt.copy(x = pt.x + 1) else (pt.copy(x = leftWall + 1))
  }

  def moveUp(pt: Point): Point = {
    if pt.y > topWall + 1 then pt.copy(y = pt.y - 1) else (pt.copy(y = bottomWall - 1))
  }

  def moveDown(pt: Point): Point = {
    if pt.y < bottomWall - 1 then pt.copy(y = pt.y + 1) else (pt.copy(y = topWall + 1))
  }

  def moveBlizzard(b: (Char, Point)): (Char, Point) = {
    b match {
      case ('<', pt) => ('<', moveLeft(pt))
      case ('>', pt) => ('>', moveRight(pt))
      case ('^', pt) => ('^', moveUp(pt))
      case ('v', pt) => ('v', moveDown(pt))
      case _ => sys.error("Wrong blizzard: " + b)
    }
  }

  val blizzardPositions = (0 to 1000).scanLeft((0, blizzards)) { case ((_, bPositions), turn) =>
    turn -> bPositions.map(moveBlizzard)
  }.toMap.mapValues(_.map(_._2).toSet)

  // We should have a list of blizzard positions for each move nr
  // Then just shortest path search:
  // neighbours are possible steps (check appropriate blizzard list for turn), including standing still
  // State has position but also turn
  // Sorting by negative manhattan distance to endpoint
  // Note that we can't simply do shortest path because paths can be circular

  val startState = State(startPt, 0)

  var visited = scala.collection.mutable.Set[State](startState)

  case class State(pos: Point, turn: Int)

  class StateOrdering extends Ordering[Path[State]] {
    def util(st: State): Int =
      -(st.turn + Point.manhattanDistance(st.pos, endPt))

    def compare(p1: Path[State], p2: Path[State]): Int = {
      val state1 = p1.endPoint
      val state2 = p2.endPoint
      util(state1).compareTo(util(state2))
    }
  }

  class Game extends WeightedGraph[State, Int] {
    override def neighbours(vertex: State): Seq[State] = {
      val blizzards = blizzardPositions(vertex.turn + 1) // CHECK: blizzards for next turn or current turn?
      val possibleMoves = (vertex.pos +: vertex.pos.neighbours.filter(validMove)).filterNot(blizzards)
      val nonVisited = possibleMoves.map { pt => State(pt, vertex.turn + 1)}.filterNot(visited)
      visited.addAll(nonVisited)
      nonVisited
    }

    override def cost(from: State, to: State): Int = 1
  }

  @main
  def day24Part1 = printSolution {
     val solution = Search.findCheapestPath[State](Game(), startState, st => st.pos == endPt, StateOrdering())
     solution.get.total + 1
  }

  @main
  def day24Part2 = printSolution {
    val solution1 = Search.findCheapestPath[State](Game(), startState, st => st.pos == endPt, StateOrdering())
    val endPt1 = solution1.get.endPoint
    val steps1 = solution1.get.total
    println ("Steps for 1: " + steps1)

    visited = scala.collection.mutable.Set[State](endPt1)
    val solution2 = Search.findCheapestPath[State](Game(), endPt1, st => st.pos == startPt, StateOrdering())
    val endPt2 = solution2.get.endPoint
    val steps2 = solution2.get.total
    println ("Steps for 2: " + steps2)

    visited = scala.collection.mutable.Set[State](endPt2)
    val solution3 = Search.findCheapestPath[State](Game(), endPt2, st => st.pos == endPt, StateOrdering())
    val endPt3 = solution3.get.endPoint
    val steps3 = solution3.get.total
    println ("Steps for 3: " + steps3)

    steps1 + steps2 + steps3 + 1
  }   // 942, 117 seconds
}
