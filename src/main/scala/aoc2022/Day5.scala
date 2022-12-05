package aoc2022


val move = """move (\d*) from (\d+) to (\d+)""".r

case class Move(nr: Int, from: Int, to: Int)

def parseMove(s: String): Move = s match {
  case move(nr, from, to) => Move(nr.toInt, from.toInt, to.toInt)
  case _ => sys.error("Unknown move: " + s)
}

def toCrate(s: String): Char = s(1)


val sc = scannerFromResource("/day5.txt")
val lines: Seq[String] = scannerToLines(sc)

val crates = lines.take(8)
val emptyStacks = List.fill(9)(List[String]())
val stacks = crates.foldLeft(emptyStacks) { case (stack, line) =>
  val cols = line.sliding(4, 4).toSeq.take(9)
  val newStacks = cols.zip(stack).map { case (col, s) =>
    if (col.trim.isEmpty) s else s :+ col
  }
  newStacks.toList
}

object Day5Part1 extends App {

  def applyMove(ss: List[List[String]], mv: Move): List[List[String]] = {
    val oldFrom = ss(mv.from-1)
    val oldTo = ss(mv.to-1)

    val newFrom = oldFrom.drop(mv.nr)
    val moving = oldFrom.take(mv.nr)
    val newTo = moving.reverse ++ oldTo

    ss.updated(mv.from-1, newFrom).updated(mv.to-1, newTo)
  }

   val finalStacks = lines.drop(10).foldLeft(stacks) {
     case (stacks , line) =>
      val move = parseMove(line)
      applyMove(stacks, move)
   }

  val solution = finalStacks.map(_.head).map(toCrate).mkString
  println(solution)
}

object Day5Part2 extends App {

  def applyMove(ss: List[List[String]], mv: Move): List[List[String]] = {
    val oldFrom = ss(mv.from - 1)
    val oldTo = ss(mv.to - 1)

    val newFrom = oldFrom.drop(mv.nr)
    val moving = oldFrom.take(mv.nr)
    val newTo = moving ++ oldTo

    ss.updated(mv.from - 1, newFrom).updated(mv.to - 1, newTo)
  }

  val finalStacks = lines.drop(10).foldLeft(stacks) {
    case (stacks, line) =>
      val move = parseMove(line)
      applyMove(stacks, move)
  }

  val solution = finalStacks.map(_.head).map(toCrate).mkString
  println(solution)
}
