package aoc2022.utils

import aoc2022.utils.Search.Path

import scala.util.Random

// NOTE: not entirely correct, steps only right or down
def cheapestPathBruteForce(g: WeightedGraph[Point, Int], from: Point, to: Point): Path[Point] = {
  def doFindCp(from: Point): Path[Point] = {
   if from == to then Path(Seq(to), 0, to)
   else
    val (nbCost, fromHere) = g.neighbours(from)
      .filter (p => p.x > from.x || p.y > from.y)
      .map { nb =>
        val costToNeighbour = g.cost(from, nb)
        (costToNeighbour, doFindCp(nb))
      }.minBy { case (cost, result) => cost + result.total }
    fromHere.copy(reverseSteps = from +: fromHere.reverseSteps, total = fromHere.total + nbCost)
  }

  val result = doFindCp(from)
  val reversedSteps = result.reverseSteps.reverse
  result.copy(reverseSteps = reversedSteps) 
}

def randomGrid(width: Int, height: Int): Grid[Int] = {
  val grid = Grid.withDimensions(width, height, initialValue = 0)
  for x <- 0 until grid.width do
    for y <- 0 until grid.height do
      grid.update(Point(x, y), Random.nextInt(10))
  grid
}

@main def testCheapestPath =
  val grid = randomGrid(12, 12)
  val cheapest = Search.findCheapestPathGreedy(grid, Point(0, 0), _ == Point(grid.width - 1, grid.height - 1))
  println(cheapest)
  Grid.printGridWithPath(grid, cheapest.get.reverseSteps)
  val cheapestBf = cheapestPathBruteForce(grid, Point(0, 0), Point(grid.width - 1, grid.height - 1))
  println(cheapestBf)
  Grid.printGridWithPath(grid, cheapestBf.reverseSteps)

@main def testShortestPath =
  val (rawGrid, start, end) = Grid.createMaze(30, 30)
  val grid = rawGrid.copy(getNeighbours = { case (g, pt) => Grid.directNeighbours(g, pt).filterNot(g.value)} ) // true = blocked
  Grid.printBooleanGrid(grid, cutEmptySpace = false)
  println()
  val shortest = Search.findShortestPath(grid, start, _ == end)
  Grid.printBooleanGridWithPath(grid, shortest.get, cutEmptySpace = false)

@main def testAllShortestPaths =
  val vertices = List("A","B","C","D")
  val adj = List(("A" -> "B", 1),("B" -> "C", 2),("C" -> "D", 3), ("A" -> "D", 2))
  println (adj)

  val solution = Search.findAllShortestPaths(vertices, adj)
  println (solution)

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

@main def testBinarySearch =
  val items = Array(-1,0,1,2,4,8,16)
  println(Search.binarySearch(items, identity, 2))
  println(Search.binarySearch(items, identity, -1))
  println(Search.binarySearch(items, identity, -2))

@main def testFindAllWith =
  val items = Array(-1, 0, 1, 2, 4, 8, 8, 8, 8, 16)
  println(Search.findAllWith(items, identity, 2))
  println(Search.findAllWith(items, identity, 8))
  println(Search.findAllWith(items, identity, 17))
