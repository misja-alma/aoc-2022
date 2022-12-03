package aoc2022

@main def testCheapestPath =
  val grid = Grid.withDimensions(3, 3, initialValue = 1)
  grid.update(Point(1,1), 5)
  grid.update(Point(2,1), 2)
  Grid.printGrid(grid)
  val cheapest = Search.findCheapestPath(grid, Point(0, 0), Point(2, 2))
  println(cheapest)

@main def testShortestPath =
  val grid = Grid.withDimensions(3, 3, initialValue = false)
  grid.update(Point(1, 1), true)
  grid.update(Point(2, 1), true)
  Grid.printBooleanGrid(grid, cutEmptySpace = false)
  val shortest = Search.findShortestPath(grid, Point(0, 0), _ == Point(2, 2))
  println(shortest)
