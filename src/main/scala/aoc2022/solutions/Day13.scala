package aoc2022.solutions

import aoc2022.utils.*

object Day13 {
  val sc = scannerFromResource("/day13.txt")
  val lines = scannerToLines(sc)

  trait IntListType
  case class IntList(var children: List[IntListType]) extends IntListType
  case class IntLeaf(value: Int) extends IntListType

  def parseList(line: String): IntListType = {
    
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

  def inOrder(ilLeft: IntListType, ilRight: IntListType): Int = {
    (ilLeft, ilRight) match {
      case (IntLeaf(vl), IntLeaf(vr)) => vl.compareTo(vr)  // vl > vr => 1 which is out of order
      case (l@IntLeaf(_), r@IntList(_)) => inOrder(IntList(List(l)), r)
      case (l@IntList(_), r@IntLeaf(_)) => inOrder(l, IntList(List(r)))
      case (iListLeft@IntList(_), iListRight@IntList(_)) =>
        // zip both; drop while undecided; if result empty, left list should be smaller than right list
        val dropUndecided = iListLeft.children.zip(iListRight.children).dropWhile { case (c1, c2) => inOrder(c1, c2) == UNDECIDED}
        if dropUndecided.isEmpty then iListLeft.children.size.compareTo(iListRight.children.size)
        else inOrder(dropUndecided.head._1, dropUndecided.head._2)
    }
  }


  @main
  def day13Part1 = {

    val correct = couples.zipWithIndex.filter { case ((l1, l2), _) =>
      val result = inOrder(l1, l2)
      result == IN_ORDER || result == UNDECIDED
    }

    val solution = correct.map { case (_, i) => i + 1}.sum
    println ("Solution: " + solution)
  }

  @main
  def day13Part2 = {
    val packetComparator = new Ordering[IntListType] {
      override def compare(x: IntListType, y: IntListType): Int = inOrder(x, y)
    }

    val dividerPackets = "[[2]]\n[[6]]".split("\n").map(parseList)
    val allPackets = couples.flatMap { case (l,r) => Seq(l,r)} ++ dividerPackets
    val sorted = allPackets.sorted(packetComparator)
    val indices = dividerPackets.map { k => sorted.indexOf(k)  + 1}
    val solution = indices.product
    println ("Solution: " + solution)
  }
}
