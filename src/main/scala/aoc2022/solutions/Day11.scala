package aoc2022.solutions

import aoc2022.utils.*

import scala.collection.mutable.ListBuffer


object Day11 {
  val sc = scannerFromResource("/day11.txt")
  val lines = scannerToLines(sc)


  case class Monkey(operation: BigInt => BigInt, items: ListBuffer[BigInt], testDivisor: BigInt, throwTrue: Int, throwFalse: Int, nr: Int)

  val plus = """\+ (-?\d+)""".r
  val multiply = """\* (-?\d+)""".r
  val square = """\* old""".r

  enum Operation:
    case Plus(term: Int) extends Operation
    case Multiply(factor: Int) extends Operation
    case Square() extends Operation

  import Operation.*

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
    }
  }

  def parseMonkey(lines: Seq[String]): Monkey = {
    val nr = lines(0).trim() match { case s"Monkey $n:" => n.toInt }
    val items = ListBuffer.from(lines(1).trim().drop("Starting items: ".length).split(',').map { i => BigInt(i.trim().toInt) })
    val operation = parseOperation(lines(2).trim().drop("Operation: new = old ".length))
    val test = lines(3).trim() match { case s"Test: divisible by $t" => t.toInt }
    val trueTo = lines(4).trim() match { case s"If true: throw to monkey $t" => t.toInt }
    val falseTo = lines(5).trim() match { case s"If false: throw to monkey $f" => f.toInt }

    Monkey(toFunction(operation), items, test, trueTo, falseTo, nr)
  }

  val blocks = split(lines, line => line.trim.isEmpty)
  val startMonkeys = blocks.map(parseMonkey)

  def calculateRounds(monkeyList: Seq[Monkey], rounds: Int, constraintFunction: BigInt => BigInt): (List[Monkey], Array[Long]) = {

    val inspected = Array.fill(monkeyList.size)(0L)
    // Monkeys are always updated immediately so it's easiest when they are mutable. But that means they need to be copied first.
    val monkeys = monkeyList.map(_.copy()).toList

    for _ <- 1 to rounds do {
      for subject <- monkeys do {
        val Monkey(operation, items, test, throwTrue, throwFalse, nr) = subject

        inspected(nr) = inspected(nr) + items.size

        for item <- items do {
          val newItem = constraintFunction(operation(item))
          val toAdd = if newItem % test == 0 then throwTrue else throwFalse
          monkeys(toAdd).items.append(newItem)
        }

        monkeys(nr).items.clear()
      }
    }

    (monkeys, inspected)
  }


  @main
  def day11Part1 = {
      val (_, inspected) = calculateRounds(startMonkeys, 20, _ / 3)

      val highest = inspected.toList.sorted.reverse.take(2)
      val solution = highest(0) * highest(1)
      println ("Solution: " + solution)
  }

  @main
  def day11Part2 = {
    val allDivisors = startMonkeys.map(_.testDivisor)
    val product = allDivisors.product

    val (_, inspected) = calculateRounds(startMonkeys, 10000, _ % product)

    val highest = inspected.toList.sorted.reverse.take(2)
    val solution = highest(0) * highest(1)
    println("Solution: " + solution)
  }
}
