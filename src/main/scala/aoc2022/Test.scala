package aoc2022

@main def testCheapestPath =
  val grid = Grid.withDimensions(3, 3, initialValue = 1)
  grid.update(Point(1,1), 5)
  grid.update(Point(2,1), 2)
  Grid.printGrid(grid)
  val cheapest = Search.findCheapestPath(grid, Point(0, 0), Point(2, 2))
  println(cheapest)
  Grid.printGridWithPath(grid, cheapest.get.reverseSteps)

@main def testShortestPath =
  val (grid, start, end) = Grid.createMaze(30, 30)
  Grid.printBooleanGrid(grid, cutEmptySpace = false)
  println()
  val shortest = Search.findShortestPath(grid, start, _ == end)
  Grid.printBooleanGridWithPath(grid, shortest.get, cutEmptySpace = false)

@main def maze =
  val (grid, start, end) = Grid.createMaze(30, 30)
  Grid.printBooleanGrid(grid, cutEmptySpace = false)
