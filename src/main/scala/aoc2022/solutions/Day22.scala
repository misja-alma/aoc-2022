package aoc2022.solutions

import aoc2022.utils.*

object Day22 {
  val sc = scannerFromResource("/day22.txt")
  val lines = scannerToLines(sc)

  val mapDirections: Seq[Seq[String]] = split(lines, line => line.trim.isEmpty)
  var pointsMap = mapDirections.head.zipWithIndex.flatMap { case (line, row) =>
    line.zipWithIndex.flatMap { case (char, col) =>
      if char != ' ' then Some(Point(col, row) -> char) else None
    }
  }.toMap

  val grid = Grid.fromPoints(pointsMap.filterNot(_._2 == '.').keys.toSeq)


  val directions = mapDirections.last.head

  val startPoint = pointsMap.keys.filter{ pt => pt.y == 0 && pointsMap(pt) == '.' }.minBy(_.x)
  val startDirection = 'R'


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
                val leftPoint = pointsMap.keys.filter { pt => pt.y == position.y }.minBy(_.x)
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
      result
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

    val State(finalPt, finalDir, _) = finalState
    println (finalState)
    val solution = 1000L * (finalPt.y + 1) + 4 * (finalPt.x + 1) + facing(finalDir)
    solution
  }// 88226

  @main
  def day22Part2 = printSolution {
    // orientation: nr of turns the face has to be turned right wrt face 1 to fit in the cube
    // nbMap: 1st is neighbour, 2nd is relative orientation of that neighbour
    case class Face(id: Int, orientation: Int, xOffset: Int, yOffset: Int, nbMap: Map[Char, (Int, Int)])

    // Cube faces by nr:
    // 12
    // 4
    //35
    //6

    // NOTE: urdl are from the perspective of the face itself! So the direction in the map
    val realFaces = Seq(
      Face(1, 0, 50, 0, Map('U' -> (6,3), 'R' -> (2,0), 'D' -> (4,0), 'L' -> (3,2))),
      Face(2, 0, 100, 0, Map('U' -> (6,0), 'R' -> (5,2), 'D' -> (4,3), 'L' -> (1,0))),
      Face(3, 2, 0, 100, Map('U' -> (4,3), 'R' -> (5,0), 'D' -> (6,0), 'L' -> (1,2))),
      Face(4, 0, 50, 50, Map('U' -> (1,0), 'R' -> (2,1), 'D' -> (5,0), 'L' -> (3,1))),
      Face(5, 2, 50, 100, Map('U' -> (4,0), 'R' -> (2,2), 'D' -> (6,3), 'L' -> (3,0))),
      Face(6, 3, 0, 150, Map('U' -> (3,0), 'R' -> (5,1), 'D' -> (2,0), 'L' -> (1,1)))
    ).map { f => f.id -> f }.toMap

    val faces = realFaces
    val faceWidth = 50
    case class State(position: Point, face: Face, direction: Char, lastMove: String)

    // The direction is assumed to be the direction wrt the points in the input. So 'L' means its entering from right and moving to left
    def selectEnteringPoint(orthogonalOffset: Int, newDirection: Char, newFace: Face, oldDirection: Char): Point = {
        val needsFlip = (oldDirection, newDirection) match {
          case ('L', 'R')|('R', 'L')|('U', 'D')|('D','U') => true
          case ('L','U')|('U','L')|('R','D')|('D','R') => true
          case _ => false
        }
        val flippedOffset = if needsFlip then (faceWidth - 1) - orthogonalOffset else orthogonalOffset
      // up, down: get y from face, adjust x for offset.
      // l,r: x from face, y adjusted
        newDirection match {
          case 'L' =>
            val newX = newFace.xOffset + (faceWidth - 1)
            val newY = newFace.yOffset + flippedOffset
            Point(newX, newY)
          case 'R' =>
            val newX = newFace.xOffset
            val newY = newFace.yOffset + flippedOffset
            Point(newX, newY)
          case 'U' =>
            val newY = newFace.yOffset + (faceWidth - 1)
            val newX = newFace.xOffset + flippedOffset
            Point(newX, newY)
          case 'D' =>
            val newY = newFace.yOffset
            val newX = newFace.xOffset + flippedOffset
            Point(newX, newY)
          case _ => sys.error("Wrong direction: " + newDirection)
        }
    }

    def directionToDiffs(direction: Char): (Int, Int) = {
      direction match {
        case 'L' =>
          (-1, 0)
        case 'R' =>
          (1, 0)
        case 'U' =>
          (0, -1)
        case 'D' =>
          (0, 1)
        case _ => sys.error("Wrong direction: " + direction)
      }
    }

    def applyDiffs(pt: Point, dx: Int, dy: Int): Point = pt.copy(x = pt.x + dx, y = pt.y + dy)

    // returns the offset orthogonal to the direction, wrt to the position of the face
    // assumes direction is the 'real' direction in the grid
    def calcOrthogonalOffset(pt: Point, face: Face, direction: Char): Int = {
      val offsetPt = pt.copy(x = pt.x - face.xOffset, y = pt.y - face.yOffset)
      direction match {
        case 'L'|'R' =>
          offsetPt.y
        case 'U'|'D' =>
          offsetPt.x
        case _ => sys.error("Wrong direction: " + direction)
      }
    }

    def insideFace(pt: Point, face: Face): Boolean = {
      pt.x >= face.xOffset && pt.x < face.xOffset + faceWidth  &&
        pt.y >= face.yOffset && pt.y < face.yOffset + faceWidth
    }

    // Returns direction, face, point
    def move(position: Point, face: Face, direction: Char, distance: Int): (Char, Face, Point) = {
      var newPt = position.copy()
      var newFace = face
      var newDirection = direction
      var blocked = false
      var movesMade = 0
      while movesMade < distance && !blocked do {
        movesMade = movesMade + 1
        val (dx, dy) = directionToDiffs(newDirection)
        val movedPt = applyDiffs(newPt, dx, dy)
        if insideFace(movedPt, newFace) then {
          if pointsMap(movedPt) == '#' then
            blocked = true
          else
            newPt = movedPt
        } else {
          // wrap around

          // determine new face
          // determine rotation + translation
          // determine what new point becomes. Translation: because faces have offsets. Rotation: because you might start at another edge
          // And then recurse
          val orthogonalOffset = calcOrthogonalOffset(newPt, newFace, newDirection)
          val (neighbour, relOrientation) = newFace.nbMap(direction)
          val myNeighbour = faces(neighbour)
          // rotate direction according to myNeighbour rotation
          val directionInNeighbour = rotateLeft(direction, relOrientation)
          // select appropriate edge of neighbour and translate position
          val pointInNeighbour = selectEnteringPoint(orthogonalOffset, directionInNeighbour, myNeighbour, direction)
          // check if blocked, if not: recurse
          if !pointsMap.contains(pointInNeighbour) then {
            println(pointInNeighbour)
          }
          if pointsMap(pointInNeighbour) == '#' then
            blocked = true
          else
            val (dirAfterMove, faceAfterMove, ptAfterMove) = move(pointInNeighbour, myNeighbour, directionInNeighbour, distance - movesMade)
            newDirection = dirAfterMove
            newFace = faceAfterMove
            newPt = ptAfterMove
            blocked = true // stop moving
        }
      }
      (newDirection, newFace, newPt)
    }

    def rotateLeft(direction: Char, turnsLeft: Int): Char =
      (0 until turnsLeft).foldLeft(direction) { case (lastDir, _) => turnLeft(lastDir) }
    
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

    val finalStateRaw = directions.foldLeft(State(startPoint, faces(1), startDirection, "")) { case (state, turn) =>
      turn match {
        case 'L' =>
          val (newDir, newFace, ptAfterMove) = if state.lastMove.nonEmpty then {
            // move in last direction
            val distance = state.lastMove.toInt
            move(state.position, state.face, state.direction, distance)
          } else sys.error("No alternation")
          state.copy(direction = turnLeft(newDir), face = newFace, position = ptAfterMove, lastMove = "")
        case 'R' =>
          val (newDir, newFace, ptAfterMove) = if state.lastMove.nonEmpty then {
            // move in last direction
            val distance = state.lastMove.toInt
            move(state.position, state.face, state.direction, distance)
          } else sys.error("No alternation")
          state.copy(direction = turnRight(newDir), face = newFace, position = ptAfterMove, lastMove = "")
        case _ => state.copy(lastMove = state.lastMove :+ turn)
      }
    }
    val finalState = if finalStateRaw.lastMove.isEmpty then finalStateRaw else {
      val (newDir, newFace, newPos) = move(finalStateRaw.position, finalStateRaw.face, finalStateRaw.direction, finalStateRaw.lastMove.toInt)
      finalStateRaw.copy(position = newPos, face = newFace, direction = newDir, lastMove = "")
    }

    val State(finalPt, _, finalDir, _) = finalState

    val solution = 1000L * (finalPt.y + 1) + 4 * (finalPt.x + 1) + facing(finalDir)
    // -> should be the facing in the map! So the one that was in the finalState already.
    solution
  } // 57305
}
