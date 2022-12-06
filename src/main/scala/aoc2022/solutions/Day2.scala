package aoc2022.solutions

import aoc2022.utils._

object Day2 {
  enum RPS:
    def value: Int = this.ordinal + 1

    case Rock extends RPS
    case Paper extends RPS
    case Scissors extends RPS


  @main
  def day2Part1 = {

    import RPS.*

    val sc = scannerFromResource("/day2.txt")
    val lines = scannerToLines(sc)

    def score(opp: RPS, me: RPS): Int = {
      val result = (opp, me) match {
        case (Rock, Rock) => 3
        case (Rock, Paper) => 6
        case (Rock, Scissors) => 0

        case (Paper, Rock) => 0
        case (Paper, Paper) => 3
        case (Paper, Scissors) => 6

        case (Scissors, Rock) => 6
        case (Scissors, Paper) => 0
        case (Scissors, Scissors) => 3
      }

      result + me.value
    }

    val scores = lines.map { line =>
      val opp = line.head match {
        case 'A' => Rock
        case 'B' => Paper
        case 'C' => Scissors
      }
      val me = line(2) match {
        case 'X' => Rock
        case 'Y' => Paper
        case 'Z' => Scissors
      }
      score(opp, me)
    }

    val solution = scores.sum
    println(solution)
  }

  @main
  def day2Part2 = {

    import RPS.*

    val sc = scannerFromResource("/day2.txt")
    val lines = scannerToLines(sc)

    def score(opp: RPS, result: Int): Int = {
      val me = (opp, result) match {
        case (Rock, 0) => Scissors
        case (Rock, 3) => Rock
        case (Rock, 6) => Paper

        case (Paper, 0) => Rock
        case (Paper, 3) => Paper
        case (Paper, 6) => Scissors

        case (Scissors, 0) => Paper
        case (Scissors, 3) => Scissors
        case (Scissors, 6) => Rock

        case _ => sys.error("Invalid combination: " + (opp, result))
      }

      result + me.value
    }

    val scores = lines.map { line =>
      val opp = line.head match {
        case 'A' => Rock
        case 'B' => Paper
        case 'C' => Scissors
      }
      val outcome = line(2) match {
        case 'X' => 0
        case 'Y' => 3
        case 'Z' => 6
      }
      score(opp, outcome)
    }

    val solution = scores.sum
    println(solution)
  }
}
