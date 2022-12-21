package aoc2022.solutions

import aoc2022.utils.*

object Day21 {
  val sc = scannerFromResource("/day21.txt")
  val lines = scannerToLines(sc)

  val plus = """([a-z]+): ([a-z]+) \+ ([a-z]+)""".r
  val multiply = """([a-z]+): ([a-z]+) \* ([a-z]+)""".r
  val minus = """([a-z]+): ([a-z]+) \- ([a-z]+)""".r
  val divide = """([a-z]+): ([a-z]+) \/ ([a-z]+)""".r
  val value = """([a-z]+): (\d+)""".r

  enum Operation:
    case Plus() extends Operation
    case Minus() extends Operation
    case Multiply() extends Operation
    case Divide() extends Operation

  import Operation.*

  def parseMonkey(op: String): Monkey = op match {
    case minus(m, m1, m2) => Monkey(m, None, Seq(m1, m2), Some(Minus()))
    case plus(m, m1, m2) => Monkey(m, None, Seq(m1, m2), Some(Plus()))
    case divide(m, m1, m2) => Monkey(m, None, Seq(m1, m2), Some(Divide()))
    case multiply(m, m1, m2) => Monkey(m, None, Seq(m1, m2), Some(Multiply()))
    case value(m, value) => Monkey(m, Some(value.toInt), Seq(), None)
    case _ => sys.error("Cant parse: " + op)
  }
  
  def apply(op: Operation, value1: Long, value2: Long): Long =
    op match {
      case Plus() => value1 + value2
      case Minus() => value1 - value2
      case Multiply() => value1 * value2
      case Divide() => value1 / value2
    }

  case class Monkey(name: String, var value: Option[Long], children: Seq[String], operation: Option[Operation])


  def evaluate(monkey: Monkey): Unit = {
      if monkey.value.isEmpty then
        val monkeyChildren = monkey.children.map(monkeyMap)
        monkeyChildren.foreach(evaluate)
        val newValue = apply(monkey.operation.get, monkeyChildren.head.value.get, monkeyChildren.last.value.get)
        monkey.value = Some(newValue)
  }


  val monkeys = lines.map(parseMonkey)
  val monkeyMap = monkeys.map { monkey => monkey.name -> monkey }.toMap
  val rootMonkey = monkeyMap("root")

  @main
  def day21Part1 = printSolution {
    evaluate(rootMonkey)

    rootMonkey.value.get
  }

  def isHumnInBranch(monkey: Monkey): Boolean =
    monkey.name == "humn" || {
      monkey.children.exists {  child =>
        isHumnInBranch(monkeyMap(child))
      }
    }



  def adjustDiffToHumnLeft(monkey: Monkey, existingDiff: Double): Double = {
    val childA = monkeyMap(monkey.children.head)
    val childB = monkeyMap(monkey.children.last)
    monkey.operation.get match {
      case Plus() => existingDiff
      case Minus() => existingDiff
      // (c * b) = (a * b) + y
      // c = ((a * b) + y) / b
      // newDiff = c - a
      case Multiply() =>
        val c = (monkey.value.get + existingDiff) / childB.value.get
        c - childA.value.get
      // (c / b) = (a / b) + y
      // c = ((a / b) + y) * b
      case Divide() =>
        val c = (monkey.value.get + existingDiff) * childB.value.get
        c - childA.value.get
    }
  }

  def adjustDiffToHumnRight(monkey: Monkey, existingDiff: Double): Double = {
    val childA = monkeyMap(monkey.children.head)
    val childB = monkeyMap(monkey.children.last)
    monkey.operation.get match {
      case Plus() => existingDiff
      case Minus() => -existingDiff
      case Multiply() =>
        val c = (monkey.value.get + existingDiff) / childA.value.get
        c - childB.value.get
      // (a / c) = (a / b) + y
      // c = 1 / (((a / b) + y) / a) = a / ((a / b) + y)
      case Divide() =>
        val c = childA.value.get / (monkey.value.get + existingDiff)
        c - childB.value.get
    }
  }

  def findValueForHumn(existingDiff: Double, startWith: Monkey):  Long = {
    if startWith.name == "humn" then
      Math.round(existingDiff + startWith.value.get)
    else {
      val childA = monkeyMap(startWith.children.head)
      val childB = monkeyMap(startWith.children.last)
      val inLeft = isHumnInBranch(childA)
      val inRight = isHumnInBranch(childB)
      if inLeft && inRight then sys.error("Monkey " + startWith + " has humn in both!")
      else {
        val newFactor =
          if inLeft then
            adjustDiffToHumnLeft(startWith, existingDiff)
          else
            adjustDiffToHumnRight(startWith, existingDiff)
        if inLeft then
          findValueForHumn(newFactor, childA)
        else
          findValueForHumn(newFactor, childB)
      }
    }
  }

  @main
  def day21Part2 = printSolution {
    evaluate(rootMonkey)  // makes sure that all monkeys ahave a value
    val childA = monkeyMap(rootMonkey.children.head)
    val childB = monkeyMap(rootMonkey.children.last)
    
    //monkeyChildren.foreach(evaluate)
    // for each branch: make sure the humn branch equals the other branch
    // this means making it larger or smaller by some term
    
    // humn is in branch left
    val diff = childB.value.get - childA.value.get
    val solution = findValueForHumn(diff, childA)
    solution
  }    // 3378273370680
}
