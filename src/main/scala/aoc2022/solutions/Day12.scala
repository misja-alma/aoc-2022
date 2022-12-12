package aoc2022.solutions

import aoc2022.utils.*

object Day12 {
  val sc = scannerFromResource("/day12.txt")
  val lines = scannerToLines(sc)


  val grid = lines.map(_.toCharArray).toArray

  case class CharGraph(grid: Array[Array[Char]]) extends Graph[Point, Boolean] {

    // TODO get rid of this, shortestpath should only check neighbours. Grid should override neighbours themselves for blocked points
    override def value(point: Point): Boolean = false

    def charValue(point: Point): Char = {
      grid(point.y)(point.x)
    }

    def filterValid(pts: Seq[Point]): Seq[Point] =
      pts.filter(p => p.x >= 0 && p.x < grid.head.length && p.y >= 0 && p.y < grid.length)

    def neighboursRaw(point: Point): Seq[Point] = {
      val raw = Seq(Point(point.x - 1, point.y), Point(point.x, point.y - 1), Point(point.x + 1, point.y), Point(point.x, point.y + 1))
      filterValid(raw)
    }

    def allPointsLazy: LazyList[Point] =
      for {
        y <- grid.indices.to(LazyList)
        x <- grid.head.indices.to(LazyList)
      } yield Point(x, y)

    def findPoint(pred: Char => Boolean): Option[Point] = {
      allPointsLazy.find(p => pred(charValue(p)))
    }

    def neighbours(point: Point): Seq[Point] = {
      val currentValue = charValue(point)
      val all = neighboursRaw(point)
      if currentValue == 'S' then all else all.filter {
        n =>
          val realValue = if charValue(n) == 'E' then 'z' else charValue(n)
          realValue.toInt <= currentValue.toInt + 1
      }
    }
  }

  val graph = CharGraph(grid)
  val start = graph.findPoint(_ == 'S').get
  val end = graph.findPoint(_ == 'E').get

  val result = Search.findShortestPath[Point](graph, start, _ == end)
  val rGrid = new Grid(grid)

  @main
  def day12Part1 = {
    Grid.printGridWithPath(rGrid, result.get)

    println("Solution: " + (result.get.size - 1))
  }

  @main
  def day12Part2 = {
    val allStartingPoints = rGrid.allPoints.filter { p => rGrid.value(p) == 'a' }
    val shortestPaths = allStartingPoints.flatMap { sp =>
      Search.findShortestPath[Point](graph, sp, _ == end)
    }
    val shortest = shortestPaths.minBy(_.size)

    println("Solution: " + (shortest.size - 1))
  }
}
