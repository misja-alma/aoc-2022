package aoc2022.solutions

import aoc2022.utils.*

object Day23 {
  val sc = scannerFromResource("/day23.txt")
  val lines = scannerToLines(sc)

  val elves = lines.zipWithIndex.flatMap { case (line, row) => line.trim.zipWithIndex.flatMap {
    case ('#', col) => Some(Point(col, row))
    case _ => None
  }}

  enum Checks:
    case North extends Checks
    case South extends Checks
    case East extends Checks
    case West extends Checks
  import Checks._

  val checkList = List(North, South, West, East)
  var state = elves.toSet

  def move(elf: Point, direction: Checks): Point =
    direction match {
      case North => elf.copy(y = elf.y - 1)
      case South => elf.copy(y = elf.y + 1)
      case West => elf.copy(x = elf.x - 1)
      case East => elf.copy(x = elf.x + 1)
    }


  // returns the old elf + the proposed move
  def proposeMove(round: Int, elves: Set[Point])(elf: Point): (Point, Point) = {
    if !elf.allNeighbours.exists(elves) then (elf, elf)
    else {
      val preferredDirection = (round until round + 4)
        .map { r => checkList((r - 1) % 4) }
        .find {
            case North =>
              !Seq(Point(elf.x - 1, elf.y - 1), Point(elf.x, elf.y - 1), Point(elf.x + 1, elf.y - 1)).exists(elves)
            case South =>
              !Seq(Point(elf.x - 1, elf.y + 1), Point(elf.x, elf.y + 1), Point(elf.x + 1, elf.y + 1)).exists(elves)
            case West =>
              !Seq(Point(elf.x - 1, elf.y - 1), Point(elf.x - 1 , elf.y), Point(elf.x - 1, elf.y + 1)).exists(elves)
            case East =>
              !Seq(Point(elf.x + 1, elf.y - 1), Point(elf.x + 1, elf.y), Point(elf.x + 1, elf.y + 1)).exists(elves)
        }

      preferredDirection match {
        case Some(direction) => (elf, move(elf, direction))
        case None => (elf, elf)
      }
    }
  }

  @main
  def day23Part1 = printSolution {
    for round <- 1 to 10 do {
      // 1st half
      val proposedMoves = state.toList.map(proposeMove(round, state))
      val proposalCount = proposedMoves.map(_._2).groupBy(identity)
      // 2nd half: for each elf/proposedMove: execute or not
      state = proposedMoves.map { case (oldElf, newElf) =>
        if proposalCount(newElf).size == 1 then newElf else oldElf
      }.toSet
    }

    //  find smallest spanning rectangle, solution is surface of rectangle - nr elves
    val width = state.maxBy(_.x).x - state.minBy(_.x).x + 1
    val height = state.maxBy(_.y).y - state.minBy(_.y).y + 1
    width * height - elves.size
  }

  @main
  def day23Part2 = printSolution {
    var round = 0
    var someMoving = true
    while someMoving do {
      round = round + 1
      someMoving = false
      // 1st half
      val proposedMoves = state.toList.map(proposeMove(round, state))
      val proposalCount = proposedMoves.map(_._2).groupBy(identity)
      // 2nd half: for each elf/proposedMove: execute or not
      state = proposedMoves.map { case (oldElf, newElf) =>
        if proposalCount(newElf).size == 1 then {
          if newElf != oldElf then someMoving = true
          newElf
        } else oldElf
      }.toSet
    }

    round
  } // 970
}
