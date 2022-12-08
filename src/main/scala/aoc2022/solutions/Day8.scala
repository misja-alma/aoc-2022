package aoc2022.solutions

import aoc2022.utils.*

object Day8 {
  val sc = scannerFromResource("/day8.txt")
  val lines = scannerToLines(sc)
  val rows = lines.map{_.trim.map(_.asDigit)}

  @main
  def day8Part1 = {
    val grid = Grid.withDimensions(rows.size, rows.head.size, false)

    rows.zipWithIndex.foreach { (line, row) =>

      val fromLeft = line.zipWithIndex.foldLeft(-1) { case (highest, (d, col)) =>
        if d > highest then grid.update(Point(row, col), true)
        if d > highest then d else highest
      }

      val fromRight = line.zipWithIndex.reverse.foldLeft(-1) { case (highest, (d, col)) =>
        if d > highest then grid.update(Point(row, col), true)
        if d > highest then d else highest
      }

      fromLeft + fromRight
    }

    (0 until rows.head.size).foreach { col =>
        val up = (0 until rows.size).foldLeft(-1) { case (highest, row) =>
          val d = rows(row)(col)
          if d > highest then grid.update(Point(row, col), true)
          if d > highest then d else highest
        }

        val down = (rows.size - 1 to 0 by -1).foldLeft(-1) { case (highest, row) =>
          val d = rows(row)(col)
          if d > highest then grid.update(Point(row, col), true)
          if d > highest then d else highest
        }

        up + down
      }

    val solution = grid.count(_ == true)
    println("Solution: " + solution)
  }

  @main
  def day8Part2 = {
    val products = (0 until rows.size).map { row =>
      (0 until rows.head.size).map { col =>
        val subj = rows(row)(col)

        var cnt = 0
        var higher = false
        for r <- row -1 to 0 by -1 do
          if !higher then cnt = cnt + 1
          if (rows(r)(col) >= subj) higher = true
        val vUpr = cnt

        cnt = 0
        higher = false
        for r <- row + 1 until rows.size do
          if !higher then cnt = cnt + 1
          if (rows(r)(col) >= subj) higher = true
        val vDownr = cnt

        cnt = 0
        higher = false
        for c <- col - 1 to 0 by - 1 do
          if !higher then cnt = cnt + 1
          if (rows(row)(c) >= subj) higher = true
        val vLeftr = cnt

        cnt = 0
        higher = false
        for c <- col + 1 until rows.head.size do
          if !higher then cnt = cnt + 1
          if (rows(row)(c) >= subj) higher = true
        val vRightr = cnt

        vUpr * vDownr * vLeftr * vRightr
      }
    }

    val solution = products.map(_.max).max
    println ("Solution: " + solution)
  }
}
