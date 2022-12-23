package aoc2022.solutions

import aoc2022.utils.*

object Day22 {
  val sc = scannerFromResource("/test22.txt")
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
    case class Face(id: Int, orientation: Int, xOffset: Int, yOffset: Int, nbMap: Map[Char, Int])
    // 12
    // 4
    //35
    //6
    // NOTE: urdl are from the perspective of the face itself! So the direction in the map
    val realFaces = Seq(
      Face(1, 0, 50, 0, Map('U' -> 6, 'R' -> 2, 'D' -> 4, 'L' -> 3)),
      Face(2, 0, 100, 0, Map('U' -> 6, 'R' -> 5, 'D' -> 4, 'L' -> 1)),
      Face(3, 2, 0, 100, Map('U' -> 4, 'R' -> 5, 'D' -> 6, 'L' -> 1)),
      Face(4, 0, 50, 50, Map('U' -> 1, 'R' -> 2, 'D' -> 5, 'L' -> 3)),
      Face(5, 2, 50, 100, Map('U' -> 4, 'R' -> 2, 'D' -> 6, 'L' -> 3)),
      Face(6, 3, 0, 150, Map('U' -> 3, 'R' -> 5, 'D' -> 2, 'L' -> 1))
    ).map { f => f.id -> f }.toMap


    //  1
    //234
    //  56
    val testFaces = Seq(
      Face(1, 0, 8, 0, Map('U' -> 2, 'R' -> 6, 'D' -> 4, 'L' -> 3)),
      Face(2, 2, 0, 4, Map('U' -> 1, 'R' -> 3, 'D' -> 5, 'L' -> 6)),
      Face(3, 1, 4, 4, Map('U' -> 1, 'R' -> 4, 'D' -> 5, 'L' -> 2)),
      Face(4, 0, 8, 4, Map('U' -> 1, 'R' -> 6, 'D' -> 5, 'L' -> 3)),
      Face(5, 2, 8, 8, Map('U' -> 4, 'R' -> 6, 'D' -> 2, 'L' -> 3)),
      Face(6, 2, 12, 8, Map('U' -> 4, 'R' -> 1, 'D' -> 2, 'L' -> 5))
    ).map { f => f.id -> f }.toMap

    val faces = testFaces
    val faceWidth = 4

    // NOTE: when traversing faces, the rotation of the other faces can change!
    // So when moving up or down, the rotation of the face left or right turns the same amount wrt the next face
    // left or right: similar for up or down
    // TODO so when traversing, we need to know:
    // - which face we are in now
    // - how much this face is turned LR, UD wrt face 1 so we can adjust the orientation of the neighbours when needed

    case class State(position: Point, face: Face, direction: Char, lastMove: String)

    def normalizeDirection(direction: Char, face: Face): Char = {
      //rotateRight(direction, face.orientation) // TODO should this be right? left? Or no rotation needed at all?
      direction
    }

    // The direction is assumed to be the direction wrt the points in the input. So 'L' means its entering from right and moving to left
    // TODO depending on the rotation of the newFace wrt the old face, it might be that the orthogonal coordinate has to flip side as well
    // CHECK: does it flip sides only after relative right rotation of new face is 2 or 3?
    def selectEnteringPoint(orthogonalOffset: Int, newDirection: Char, newFace: Face, oldFace: Face): Point = {
        val relOrientation = (newFace.orientation - oldFace.orientation + 4) % 4
        val needsFlip = relOrientation == 2 || relOrientation == 3
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

          // TODO when we go down/up, the rotations of faces left and right of us, relative to us, spin 1!
          //      similarly for down/up when we move left/right

          // translation + rotation: position.copy(x = finalX)
          // determine what new point becomes. Translation: because faces have offsets. Rotation: because you might start at another edge
          // And then recurse
          val orthogonalOffset = calcOrthogonalOffset(newPt, newFace, newDirection)
          val directionInMap = normalizeDirection(direction, newFace) // TODO not needed
          val neighbour = newFace.nbMap(directionInMap)
          val myNeighbour = faces(neighbour)
          // rotate direction according to myNeighbour rotation
          // TODO adjust rotation for nr of lr's ud's from top face
          val directionInNeighbour = rotateLeft(directionInMap, myNeighbour.orientation)
          // select appropriate edge of neighbour and translate position
          // TODO pass in adjusted neigbour orientation
          val pointInNeighbour = selectEnteringPoint(orthogonalOffset, directionInNeighbour, myNeighbour, newFace)
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

    def rotateRight(direction: Char, turnsRight: Int): Char =
      (0 until turnsRight).foldLeft(direction) { case (lastDir, _) => turnRight(lastDir) }

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

    // TODO remove all blocking points
    //pointsMap = pointsMap.map { case (pt, ch) => pt -> '.'}

    val finalStateRaw = directions.foldLeft(State(startPoint, faces(1), startDirection, "")) { case (state, turn) =>
      turn match {
        case 'L' =>
          val (newDir, newFace, ptAfterMove) = if state.lastMove.nonEmpty then {
            // move in last direction
            val distance = state.lastMove.toInt
            move(state.position, state.face, state.direction, distance)
          } else sys.error("No alternation")
          state.copy(direction = turnLeft(newDir), face = newFace, position = ptAfterMove, lastMove = "") // TODO is turnLeft also turnleft on the bottom?
        case 'R' =>
          val (newDir, newFace, ptAfterMove) = if state.lastMove.nonEmpty then {
            // move in last direction
            val distance = state.lastMove.toInt
            move(state.position, state.face, state.direction, distance)
          } else sys.error("No alternation")
          state.copy(direction = turnRight(newDir), face = newFace, position = ptAfterMove, lastMove = "") // TODO is turnRight also turnRight on the bottom?
        case _ => state.copy(lastMove = state.lastMove :+ turn)
      }
    }
    val finalState = if finalStateRaw.lastMove.isEmpty then finalStateRaw else {
      val (newDir, newFace, newPos) = move(finalStateRaw.position, finalStateRaw.face, finalStateRaw.direction, finalStateRaw.lastMove.toInt)
      finalStateRaw.copy(position = newPos, face = newFace, direction = newDir, lastMove = "")
    }

    // TODO make some test input files, to check the walks
    // test 1 should end up on top of face 3
    // face 3 is turned 2 to the right. So coordinate wise that means it starts at x = 0 and starts walking to right

    val State(finalPt, _, finalDir, _) = finalState
    println(finalState)
    val solution = 1000L * (finalPt.y + 1) + 4 * (finalPt.x + 1) + facing(finalDir)
    // TODO the direction for facing needs to be different! In the problem, 'up' means up in the z-coordinate if you imagine it's a real cube with face 1 on top
    // -> should be the facing in the map! So the one that was in the finalState already.
    solution
  } // 101024 is false (too high), 44335 too low


  // 12
  // 4
  //35
  //6


}
