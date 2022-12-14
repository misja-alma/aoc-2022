package aoc2022.solutions

import aoc2022.utils.*

object Day8 {
  val sc = scannerFromResource("/day8.txt")
  val lines = scannerToLines(sc)
  val rows = lines.map{_.trim.map(_.asDigit)}
  val grid = Grid.fromRows(rows)
  val cols = grid.columns

  @main
  def day8Part1 = printSolution {
    val visibleGrid = Grid.withDimensions(rows.size, rows.head.size, initialValue = false)

    rows.zipWithIndex.foreach { (line, row) =>
      line.zipWithIndex.foldLeft(-1) { case (highest, (d, col)) =>
        if d > highest then visibleGrid.update(Point(row, col), true)
        if d > highest then d else highest
      }

      line.zipWithIndex.reverse.foldLeft(-1) { case (highest, (d, col)) =>
        if d > highest then visibleGrid.update(Point(row, col), true)
        if d > highest then d else highest
      }
    }

    cols.zipWithIndex.foreach { (line, col) =>
      line.zipWithIndex.foldLeft(-1) { case (highest, (d, row)) =>
        if d > highest then visibleGrid.update(Point(row, col), true)
        if d > highest then d else highest
      }

      line.zipWithIndex.reverse.foldLeft(-1) { case (highest, (d, row)) =>
        if d > highest then visibleGrid.update(Point(row, col), true)
        if d > highest then d else highest
      }
    }

    visibleGrid.count(_ == true)
  }

  @main
  def day8Part2 = printSolution {
    val products = grid.allPoints.map { case point@Point(col, row) =>
      val subject = grid.value(point)

      def countVisible(viewFromSubject: Seq[Point]): Int = {
        val unBlocked = viewFromSubject.indexWhere(grid.value(_) >= subject)
        if unBlocked >= 0 then unBlocked + 1 else viewFromSubject.size
      }

      val upPoints = (row - 1 to 0 by -1).map(Point(col, _))
      val vUp = countVisible(upPoints)

      val downPoints = (row + 1 until grid.height).map(Point(col, _))
      val vDown = countVisible(downPoints)

      val leftPoints = (col - 1 to 0 by -1).map(Point(_, row))
      val vLeft = countVisible(leftPoints)

      val rightPoints = (col + 1 until grid.width).map(Point(_, row))
      val vRight = countVisible(rightPoints)

      vUp * vDown * vLeft * vRight
    }

    products.max
  }
}
