package aoc2022.solutions

import aoc2022.utils.*

import scala.annotation.tailrec

object Day13 {
  val sc = scannerFromResource("/day13.txt")
  val lines = scannerToLines(sc)

  trait IntListType
  case class IntList(var children: List[IntListType]) extends IntListType
  case class IntLeaf(value: Int) extends IntListType

  def parseList(line: String): IntListType = {
    
    @tailrec
    def doParse(ln: String, stack: List[IntListType]): List[IntListType] = {
      if ln.isEmpty then
        stack
      else if ln.startsWith(",") then
        doParse(ln.tail, stack)
      else if ln.startsWith("[") then
        val newList = IntList(List())
        val newStack = newList +: stack
        doParse(ln.tail, newStack)
      else if ln.startsWith("]") then
        val readyList = stack.head
        val newStack = stack.tail
        if newStack.nonEmpty then
          val parent = newStack.head.asInstanceOf[IntList]
          parent.children = parent.children :+ readyList
          doParse(ln.tail, newStack)
        else
          if ln.length > 1 then
            sys.error("Stack empty but string left! " + ln.tail)
          else
            doParse(ln.tail, stack)
      else // read value
        val valueChars = ln.takeWhile { c => c != ',' && c != ']'}.mkString
        val newLeaf = IntLeaf(valueChars.toInt)
        if stack.nonEmpty then
          val parent = stack.head.asInstanceOf[IntList]
          parent.children = parent.children :+ newLeaf
          doParse(ln.drop(valueChars.length), stack)
        else    //this should never happen
          sys.error("Intleaf parsed without parent!")
    }

    val finalStack = doParse(line, List())
    if finalStack.size != 1 then sys.error("Wrong stack!")
    finalStack.head
  }

  val linePairs = split(lines, l => l.trim().isEmpty)
  val couples: Seq[(IntListType, IntListType)] = linePairs.map { lp =>
    (parseList(lp.head.trim()), parseList(lp.last.trim()))
  }

  val OUT_OF_ORDER = 1
  val IN_ORDER = -1
  val UNDECIDED = 0

  def comparePackets(ilLeft: IntListType, ilRight: IntListType): Int = {
    (ilLeft, ilRight) match {
      case (IntLeaf(vl), IntLeaf(vr)) => vl.compareTo(vr)
      case (l@IntLeaf(_), r@IntList(_)) => comparePackets(IntList(List(l)), r)
      case (l@IntList(_), r@IntLeaf(_)) => comparePackets(l, IntList(List(r)))
      case (iListLeft@IntList(_), iListRight@IntList(_)) =>
        val dropUndecided = iListLeft.children.zip(iListRight.children).dropWhile { case (c1, c2) => comparePackets(c1, c2) == UNDECIDED}
        if dropUndecided.isEmpty then iListLeft.children.size.compareTo(iListRight.children.size)
        else comparePackets(dropUndecided.head._1, dropUndecided.head._2)
    }
  }


  @main
  def day13Part1 = {
    val correct = couples.zipWithIndex.filter { case ((l1, l2), _) =>
      comparePackets(l1, l2) == IN_ORDER
    }

    val solution = correct.map { case (_, i) => i + 1}.sum
    println ("Solution: " + solution)
  }

  @main
  def day13Part2 = {
    val packetComparator = new Ordering[IntListType] {
      override def compare(x: IntListType, y: IntListType): Int = comparePackets(x, y)
    }

    val dividerPackets = "[[2]]\n[[6]]".split("\n").map(parseList)
    val allPackets = couples.flatMap { case (l,r) => Seq(l,r)} ++ dividerPackets
    val sorted = allPackets.sorted(packetComparator)
    val indices = dividerPackets.map { k => sorted.indexOf(k)  + 1}
    val solution = indices.product
    println ("Solution: " + solution)
  }
}
