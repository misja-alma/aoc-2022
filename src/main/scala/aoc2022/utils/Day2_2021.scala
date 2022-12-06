package aoc2022.utils

import aoc2022.utils.*
import cats.implicits.*

object Day22021Part1 extends App {

  case class Position(forward: Int, aim: Int, depth: Int)

  val forward = """forward (\d*)""".r
  val up = """up (\d*)""".r
  val down = """down (\d*)""".r

  type Move = Position => Position

  def parseMove(s: String): Either[IllegalArgumentException, Move] = s match {
    case forward(dx) => Right(pos => pos.copy(forward = pos.forward + dx.toInt))
    case up(dz) =>      Right(pos => pos.copy(depth = pos.depth - dz.toInt))
    case down(dz) =>    Right(pos => pos.copy(depth = pos.depth + dz.toInt))
    case _ =>           Left(new IllegalArgumentException("Unknown move: " + s))
  }

  val sc = scannerFromResource("/day2_2021.txt")
  val movesOrError: Either[IllegalArgumentException, Seq[Move]] = scannerToLines(sc).traverse(parseMove)
  val moves = movesOrError.getOrElse(sys.error(movesOrError.toString))
  val result = moves.foldLeft(Position(0, 0, 0)) { case (pos, cmd) => cmd(pos) }
  println("Solution: " + result.forward * result.depth)
}

