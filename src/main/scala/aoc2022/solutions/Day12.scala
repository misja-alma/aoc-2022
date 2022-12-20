package aoc2022.solutions

import aoc2022.utils.*

object Day12 {
  val sc = scannerFromResource("/day12.txt")
  val lines = scannerToLines(sc)

  val grid = lines.map(_.toCharArray).toArray

  val graph = Grid(grid).copy(getNeighbours = { case (g, point) =>
    val currentValue = g.value(point)
    val all = Grid.directNeighbours(g, point)
    if currentValue == 'S' then all else all.filter {
      n =>
        val realValue = if g.value(n) == 'E' then 'z' else g.value(n)
        realValue.toInt <= currentValue.toInt + 1
    }
  })
  val start = graph.findPoint(_ == 'S').get
  val end = graph.findPoint(_ == 'E').get

  @main
  def day12Part1 = printSolution {
    val result = Search.findShortestPath[Point](graph, start, _ == end)
    result.get.size - 1
  }

  @main
  def day12Part2 = printSolution {
    val allStartingPoints = graph.allPoints.filter { p => graph.value(p) == 'a' }
    val shortestPaths = allStartingPoints.flatMap { sp =>
      Search.findShortestPath[Point](graph, sp, _ == end)
    }
    val shortest = shortestPaths.minBy(_.size)

    shortest.size - 1
  }
}
