package aoc2022.solutions

import aoc2022.utils.*

import scala.collection.mutable.ListBuffer


object Day11 {
  val sc = scannerFromResource("/day11.txt")
  val lines = scannerToLines(sc)


  case class Monkey(operation: BigInt => BigInt, items: List[BigInt], testDivisor: BigInt, throwTrue: Int, throwFalse: Int, nr: Int)

  val plus = """\+ (-?\d+)""".r
  val multiply = """\* (-?\d+)""".r
  val square = """\* old""".r

  enum Operation:
    case Plus(term: Int) extends Operation
    case Multiply(factor: Int) extends Operation
    case Square() extends Operation

  import Operation._

  def parseOperation(op: String): Operation = op match {
    case square() => Square()
    case plus(target) => Plus(target.toInt)
    case multiply(target) => Multiply(target.toInt)
    case _ => sys.error("Cant parse: " + op)
  }

  def toFunction(op: Operation): BigInt => BigInt = {
    op match {
      case Square() => x => x * x
      case Plus(term) => _ + term
      case Multiply(factor) => _ * factor
      case _ => sys.error("Unknow operation: " + op)
    }
  }


  def parseMonkey(lines: Seq[String]): Monkey = {
    val nr = lines(0).trim().drop("Monkey ".size).head.asDigit
    val items = lines(1).trim().drop("Starting items: ".size).split(',').map { i => BigInt(i.trim().toInt) }.toList
    val operation = parseOperation(lines(2).trim().drop("Operation: new = old ".size))
    val test = lines(3).trim().drop("Test: divisible by ".size).toInt
    val trueTo = lines(4).trim().drop("If true: throw to monkey ".size).toInt
    val falseTo = lines(5).trim().drop("If false: throw to monkey ".size).toInt

    Monkey(toFunction(operation), items, test, trueTo, falseTo, nr)
  }

  val blocks = split(lines, line => line.trim.isEmpty)
  val startMonkeys = blocks.map(parseMonkey).toList


  // TODO better to use mutable array here, since everything always needs to be updated at once

  def calculateRounds(rounds: Int, constraintFunction: BigInt => BigInt): (List[Monkey], Array[Long]) = {

    val inspected = Array.fill(8)(0L)
    var monkeys = startMonkeys

    for _ <- 1 to rounds do {
      val roundResult = monkeys.foldLeft(monkeys) { case (prevMonkeys, Monkey(_, _, _, _, _, nr)) =>

        val subject = prevMonkeys(nr)
        val Monkey(operation, items, test, throwTrue, throwFalse, _) = subject

        inspected(nr) = inspected(nr) + items.size

        val newMonkeys = items.foldLeft(prevMonkeys) { (oldMonkeys, item) =>

          val newItem = constraintFunction(operation(item))

          val toAdd = if newItem % test == 0 then throwTrue else throwFalse

          val addTo = oldMonkeys(toAdd)

          val newTarget = addTo.copy(items = addTo.items :+ newItem)
          val newMonkeys = ListBuffer.from(oldMonkeys)
          newMonkeys.update(toAdd, newTarget)
          newMonkeys.toList
        }

        val newerMonkeys = ListBuffer.from(newMonkeys)
        newerMonkeys.update(nr, subject.copy(items = List()))
        newerMonkeys.toList
      }

      monkeys = roundResult
    }

    (monkeys, inspected)
  }
  

  @main
  def day11Part1 = {
      val (_, inspected) = calculateRounds(20, _ / 3)

      val highest = inspected.toList.sorted.reverse.take(2)
      val solution = highest(0) * highest(1)
      println ("Solution: " + solution)    // 151312
  }

  @main
  def day11Part2 = {
    val allDivisors = startMonkeys.map(_.testDivisor)
    val product = allDivisors.product

    val (_, inspected) = calculateRounds(10000, _ % product)

    val highest = inspected.toList.sorted.reverse.take(2)
    val solution = highest(0) * highest(1)
    println("Solution: " + solution)
    // 51382025916
  }
}
