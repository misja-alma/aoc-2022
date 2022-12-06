package aoc2022.utils

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

@main def testMaze =
  val (grid, start, end) = Grid.createMaze(30, 30)
  Grid.printBooleanGrid(grid, cutEmptySpace = false)

@main def testInterval =
  val i1 = Interval(1, 5)
  val i2 = Interval(2, 3)
  val i3 = Interval(0, 2)
  val i4 = Interval(4, 6)
  println (i1.intersect(i2))
  println (i1.intersect(i3))
  println (i1.intersect(i4))
  println (i4.intersect(i1))
  val i5 = Interval(-1, 1)
  val i6 = Interval(-1, 6)
  val i7 = Interval(-5, -4)
  println (i1.intersect(i5))
  println (i1.intersect(i6))
  println (i1.intersect(i7))

@main def testFrequencyMap =
  val map = FrequencyMap[String]()
  val counts = map.addCount("foo", 2)
    .addCount("bar", 3)
    .addCount("foo", 4)
  println (counts)

@main def testIterate =
  val powers = iterate[Int](1) { total => total * 2 }
  println (powers(10))
