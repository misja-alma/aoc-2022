package aoc2022.solutions

import aoc2022.utils.*

object Day25 {
  val sc = scannerFromResource("/day25.txt")
  val lines = scannerToLines(sc)

  def fromSnafu(s: String): Long =
    s.zipWithIndex.map { case (c, i) =>
      val power5 = Math.pow(5, s.length - i - 1).toLong
      val translated = c match {
        case '=' => -2
        case '-' => -1
        case '0' => 0
        case '1' => 1
        case '2' => 2
        case _ => sys.error("Not a Snafu char: " + c)
      }
      translated * power5
    }.sum

  def toBase5(i: Long): String = {
    def toBase5Recurse(i: Long): String = {
      if i == 0 then "" else toBase5Recurse(i / 5) ++ (i % 5).toString
    }

    if i == 0 then "0"
    else toBase5Recurse(i)
  }

  def toSnafu(i: Long): String = {
    val (result, carry) = toBase5(i).reverse.foldLeft(("", 0)) { case ((str, carry), c) =>
      val digitToMake = carry + c.asDigit
      val snafuDigit = digitToMake % 5
      val (newStr, newCarry) = snafuDigit match {
        case 0 => ('0' +: str, 0)
        case 1 => ('1' +: str, 0)
        case 2 => ('2' +: str, 0)
        case 3 => ('=' +: str, 1)
        case 4 => ('-' +: str, 1)
        case _ => sys.error("Can't handle: " + digitToMake)
      }
      (newStr, newCarry + digitToMake / 5)
    }
    val remainder = if carry > 0 then toSnafu(carry) else ""
    remainder ++ result.mkString
  }

  @main
  def day25Part1 = printSolution {
    val summed = lines.map(_.trim).map(fromSnafu).sum
    toSnafu(summed)
  }

}
