package aoc2022.utils

import scala.collection.mutable.ListBuffer

object Countle {
  enum Operator:
    def calculate(i1: Int, i2: Int): Option[Int] = this match
      case Plus => Some(i1 + i2)
      case Minus => if i1 >= i2 then Some(i1 - i2) else None
      case Mul => Some(i1 * i2)
      case Div => if i2 != 0 && i1 % i2 == 0 then Some(i1 / i2) else None

    override def toString: String = this match
      case Plus => "+"
      case Minus => "-"
      case Mul => "*"
      case Div => "/"

    case Plus extends Operator
    case Minus extends Operator
    case Mul extends Operator
    case Div extends Operator

  case class Step(int1: Int, int2: Int, op: Operator) {
    override def equals(obj: Any): Boolean =
      obj.isInstanceOf[Step] && Set(int1, int2) == Set(obj.asInstanceOf[Step].int1, obj.asInstanceOf[Step].int2)

    override def hashCode(): Int = if int1 > int2 then int1 * 65535 + int2 else int2 * 65535 + int1
  }

  // game is finished if ints contains target
  // neighbours are all valid combinations of 2 ints + 1 combinator -> result in a new step, 2 ints less from ints + 1 new int added
  case class CountleState(ints: ListBuffer[Int], reverseSteps: List[Step])

  object CountleGraph extends Graph[CountleState, Boolean] {

    def neighbours(edge: CountleState): Seq[CountleState] =
      // take all unique ordered pairs of 2 out of ints
      // combine with all operators, filter valid
      // create new states out of results
      val uniquePairs = edge.ints.permutations.map(_.take(2)).distinct
      val validSteps = uniquePairs.flatMap { ps =>
        val i1 = ps.head
        val i2 = ps.last
        Operator.values.flatMap { op =>
          op.calculate(i1, i2).map { result =>
            (result, Step(i1, i2, op))
          }
        }
      }.distinct

      validSteps.map { case (result, s) =>
        val newInts = edge.ints.clone()
        newInts -= s.int1
        newInts -= s.int2
        newInts.addOne(result)
        CountleState(newInts, s +: edge.reverseSteps)
      }.toSeq

    def value(edge: CountleState): Boolean = false // edge.reverseSteps.size
  }

  def success(target: Int): CountleState => Boolean =
    s => s.ints.contains(target)

  def printSolution(path: Seq[CountleState]): Unit = {
    for step <- path.last.reverseSteps.reverse do
      println(s"${step.int1} ${step.op} ${step.int2} = ${step.op.calculate(step.int1, step.int2).get}")
  }

  @main
  def solve = {
    val startState = CountleState(ListBuffer(75, 100, 50, 25, 4, 4), List())
    val target = 476

    Search.findShortestPath(CountleGraph, startState, success(target)) match {
      case None => println("No Solution!")
      case Some(path) => printSolution(path)
    }
  }
}
