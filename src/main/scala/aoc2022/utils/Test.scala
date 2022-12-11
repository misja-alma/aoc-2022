package aoc2022.utils

import scala.util.Random

@main def testCheapestPath =
  val grid = Grid.withDimensions(30, 30, initialValue = 0)
  for x <- 0 until grid.width do
    for y <- 0 until grid.height do
      grid.update(Point(x, y), Random.nextInt(10))
  val cheapest = Search.findCheapestPath(grid, Point(0, 0), Point(grid.width - 1, grid.height - 1))
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
  val powers = LazyList.iterate[Int](1) { total => total * 2 }
  println (powers(10))
  // Note: LazyList.dropWhile.head can also be done using LazyList.unfold

@main def testSplit =
  val seq = Seq(0,1,2,3,4,5,6,7,8,9)
  val res = split(seq, x => x % 3 == 0, true)
  println(res)
  val res2 = split(seq, x => x % 3 == 0, false)
  println(res2)
