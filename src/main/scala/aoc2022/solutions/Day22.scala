package aoc2022.solutions

import aoc2022.utils.*

object Day22 {
  val sc = scannerFromResource("/day22.txt")
  val lines = scannerToLines(sc)

  val mapDirections: Seq[Seq[String]] = split(lines, line => line.trim.isEmpty)
  val pointsMap = mapDirections.head.zipWithIndex.flatMap { case (line, row) =>
    line.zipWithIndex.flatMap { case (char, col) =>
      if char != ' ' then Some(Point(col, row) -> char) else None
    }
  }.toMap

  val grid = Grid.fromPoints(pointsMap.filterNot(_._2 == '.').keys.toSeq)


  val directions = mapDirections.last.head

  val startPoint = pointsMap.keys.filter{ pt => pt.y == 0 && pointsMap(pt) == '.' }.minBy(_.x)
  val startDirection = 'R'
  case class State(position: Point, direction: Char, lastMove: String)

  def move(position: Point, direction: Char, distance: Int): Point = {
    val result = direction match {
      case 'R' =>
        var finalX = position.x
        var blocked = false
        var movesMade = 0
        while movesMade < distance && !blocked do {
          movesMade = movesMade + 1
          finalX =
            if pointsMap.contains(Point(finalX + 1, position.y)) then {
              if pointsMap(Point(finalX + 1, position.y)) == '#' then
                blocked = true
                finalX
              else
                finalX + 1
            } else {
              // wrap around
              val leftPoint = pointsMap.keys.filter{ pt => pt.y == position.y }.minBy(_.x)
              if pointsMap(leftPoint) == '#' then
                blocked = true
                finalX
              else
                leftPoint.x
            }
        }
        position.copy(x = finalX)
      case 'D' =>
        var finalY = position.y
        var blocked = false
        var movesMade = 0
        while movesMade < distance && !blocked do {
          movesMade = movesMade + 1
          finalY =
            if pointsMap.contains(Point(position.x, finalY + 1)) then {
              if pointsMap(Point(position.x, finalY + 1)) == '#' then
                blocked = true
                finalY
              else
                finalY + 1
            } else {
              // wrap around
              val topPoint = pointsMap.keys.filter { pt => pt.x == position.x }.minBy(_.y)
              if pointsMap(topPoint) == '#' then
                blocked = true
                finalY
              else
                topPoint.y
            }
        }
        position.copy(y = finalY)
      case 'L' =>
        var finalX = position.x
        var blocked = false
        var movesMade = 0
        while movesMade < distance && !blocked do {
          movesMade = movesMade + 1
          finalX =
            if pointsMap.contains(Point(finalX - 1, position.y)) then {
              if pointsMap(Point(finalX - 1, position.y)) == '#' then
                blocked = true
                finalX
              else
                finalX - 1
            } else {
              // wrap around
              val rightPoint = pointsMap.keys.filter { pt => pt.y == position.y }.maxBy(_.x)
              if pointsMap(rightPoint) == '#' then
                blocked = true
                finalX
              else
                rightPoint.x
            }
        }
        position.copy(x = finalX)
      case 'U' =>
        var finalY = position.y
        var blocked = false
        var movesMade = 0
        while movesMade < distance && !blocked do {
          movesMade = movesMade + 1
          finalY =
            if pointsMap.contains(Point(position.x, finalY - 1)) then {
              if pointsMap(Point(position.x, finalY - 1)) == '#' then
                blocked = true
                finalY
              else
                finalY - 1
            } else {
              // wrap around
              val bottomPoint = pointsMap.keys.filter { pt => pt.x == position.x }.maxBy(_.y)
              if pointsMap(bottomPoint) == '#' then
                blocked = true
                finalY
              else
                bottomPoint.y
            }
        }
        position.copy(y = finalY)
      case _ => sys.error("Wrong facing: " + direction)
    }
//    println (s"---- Move from $position direction $direction distance $distance ----")
//
//    Grid.printBooleanGridWithPath(grid, createPath(position, result))
//
//    println()
    result
  }

  def createPath(from: Point, to: Point): Seq[Point] = {
    if from.x != to.x then
      (from.x to to.x by Math.signum(to.x - from.x).toInt).map { x => Point(x, from.y)}
    else
      if from.y != to.y then
        (from.y to to.y by Math.signum(to.y - from.y).toInt).map { y => Point(from.x, y)}
      else
        Seq(from)
  }

  def turnLeft(direction: Char): Char =
    direction match {
      case 'R' => 'U'
      case 'D' => 'R'
      case 'L' => 'D'
      case 'U' => 'L'
      case _ => sys.error("Wrong facing: " + direction)
    }

  def turnRight(direction: Char): Char =
    direction match {
      case 'R' => 'D'
      case 'D' => 'L'
      case 'L' => 'U'
      case 'U' => 'R'
      case _ => sys.error("Wrong facing: " + direction)
    }

  val finalStateRaw = directions.foldLeft(State(startPoint, startDirection, "")) { case (state, turn) =>
    turn match {
      case 'L' =>
        val ptAfterMove = if state.lastMove.nonEmpty then {
          // move in last direction
          val distance = state.lastMove.toInt
          move(state.position, state.direction, distance)
        } else sys.error("No alternation")
        state.copy(direction = turnLeft(state.direction), position = ptAfterMove, lastMove = "")
      case 'R' =>
        val ptAfterMove = if state.lastMove.nonEmpty then {
          // move in last direction
          val distance = state.lastMove.toInt
          move(state.position, state.direction, distance)
        } else sys.error("No alternation")
        state.copy(direction = turnRight(state.direction), position = ptAfterMove, lastMove = "")
      case _ => state.copy(lastMove = state.lastMove :+ turn)
    }
  }
  val finalState = if finalStateRaw.lastMove.isEmpty then finalStateRaw else {
    val newPos = move(finalStateRaw.position, finalStateRaw.direction, finalStateRaw.lastMove.toInt)
    finalStateRaw.copy(position = newPos, lastMove = "")
  }

  def facing(direction: Char): Int =
    direction match {
      case 'R' => 0
      case 'D' => 1
      case 'L' => 2
      case 'U' => 3
      case _ => sys.error("Wrong facing: " + direction)
    }


  @main
  def day22Part1 = printSolution {
    val State(finalPt, finalDir, _) = finalState
    println (finalState)
    val solution = 1000L * (finalPt.y + 1) + 4 * (finalPt.x + 1) + facing(finalDir)
    solution
  }// 88226

  @main
  def day22Part2 = printSolution {


  }
}
