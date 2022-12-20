package aoc2022.utils

import scala.collection.mutable
import scala.reflect.ClassTag
import scala.util.Random

object Grid {
  val squareBlockChar = 0x2588.toChar

  def fromRows[T: ClassTag](rows: Seq[Seq[T]]): Grid[T] = {
    new Grid(rows.map(_.toArray).toArray)
  }

  def withDimensions[T: ClassTag](x: Int, y: Int, initialValue: T): Grid[T] = {
    val ar = (0 until y).map(_ => Array.fill[T](x)(initialValue)).toArray
    new Grid(ar)
  }

  def fromPoints(points: Seq[Point]): Grid[Boolean] = {
    val maxX = points.maxBy(_.x).x
    val maxY = points.maxBy(_.y).y
    val grid = Grid.withDimensions(maxX + 1, maxY + 1, false)
    points.foreach { p => grid.update(p, true) }
    grid
  }

  def infiniteFromPoints[T: ClassTag](points: Seq[Point], pointValue: T, defaultValue: T): MapGrid[T] = {
    val ptsMap = collection.mutable.Map[Point, T](points.map ( _ -> pointValue ): _*)
    new MapGrid[T](ptsMap, defaultValue)
  }

  /**
   * Horizontal or vertical neighbours that are in the grid
   */
  def directNeighbours(grid: IGrid[?], point: Point): Seq[Point] = {
    val raw = point.neighbours
    grid.filterValid(raw)
  }

  /**
   * Horizontal, vertical or diagonal neighbours in the grid
   */
  def allNeighbours(grid: Grid[?], point: Point): Seq[Point] = {
    val raw = for {
      px <- point.x - 1 to point.x + 1
      py <- point.y - 1 to point.y + 1
      if px != point.x || py != point.y
    } yield Point(px, py)

    grid.filterValid(raw)
  }

  def printBooleanGrid(grid: IGrid[Boolean], cutEmptySpace: Boolean = true, blockChar: Char = '#'): Unit = {
    val (maxX, maxY) = if cutEmptySpace then
      val allFilled = grid.allPoints.filter { p => grid.value(p) }
      (allFilled.maxBy(_.x).x, allFilled.maxBy(_.y).y)
    else
      (grid.width - 1, grid.height - 1)

    for row <- 0 to maxY do
      for col <- 0 to maxX do
        if grid.value(Point(col, row)) then print(blockChar) else print('.')
      println()
  }

  private def dotOrPath(gridValue: Boolean, onPath: Boolean, blockChar: Char): String = {
    val normalChar = if gridValue then blockChar else '.'
    if onPath then Console.GREEN_B + normalChar +  Console.RESET else normalChar.toString
  }

  private def valueOrPath[T](gridValue: T, onPath: Boolean): String = {
    if onPath then Console.GREEN + gridValue +  Console.RESET else gridValue.toString
  }

  def printBooleanGridWithPath(grid: Grid[Boolean], path: Seq[Point], cutEmptySpace: Boolean = true, blockChar: Char = '#'): Unit = {
    val (maxX, maxY) = if cutEmptySpace then
      val allFilled = grid.allPoints.filter { p => grid.value(p) }
      (allFilled.maxBy(_.x).x, allFilled.maxBy(_.y).y)
    else
      (grid.width - 1, grid.height - 1)

    val points = path.toSet

    for row <- 0 to maxY do
      for col <- 0 to maxX do
        val pt = Point(col, row)
        val toPrint = dotOrPath(grid.value(pt), points.contains(pt), blockChar)
        print(toPrint)
      println()
  }

  def printGrid(grid: Grid[?]): Unit = {
    val maxX = grid.width
    val maxY = grid.height
    for y <- 0 until maxY do
      for x <- 0 until maxX do
        print(grid.value(Point(x, y)))
      println()
  }

  def printGridWithPath(grid: Grid[?], path: Seq[Point], pathColor: String = Console.GREEN): Unit = {
    val points = path.toSet
    val maxX = grid.width
    val maxY = grid.height
    for y <- 0 until maxY do
      for x <- 0 until maxX do
        val pt = Point(x, y)
        val toPrint = valueOrPath(grid.value(pt), points.contains(pt))
        print(toPrint)
      println()
  }

  /**
   * Creates a random maze, returns the grid, start, end
   */
  def createMaze(width: Int, height: Int): (Grid[Boolean], Point, Point) = {
    // fill maze with empty space
    val grid = Grid.withDimensions(width, height, initialValue = false)

    def drawVerticalWall(grid: Grid[Boolean], x: Int, yInterval: Interval): Unit = {
      for y <- yInterval.min to yInterval.max do
        grid.update(Point(x, y), true)
      // don't block doors
      if !grid.value(Point(x, yInterval.min - 1)) then grid.update(Point(x, yInterval.min), false)
      if !grid.value(Point(x, yInterval.max + 1)) then grid.update(Point(x, yInterval.max), false)
    }

    def drawHorizontalWall(grid: Grid[Boolean], y: Int, xInterval: Interval): Unit = {
      for x <- xInterval.min to xInterval.max do
        grid.update (Point(x, y), true)
      // don't block doors
      if !grid.value(Point(xInterval.min - 1, y)) then grid.update(Point(xInterval.min, y), false)
      if !grid.value(Point(xInterval.max + 1, y)) then grid.update(Point(xInterval.max, y), false)
    }

    // make walls around border
    for x <- 0 until width do
      grid.update(Point(x, 0), true)
      grid.update(Point(x, height - 1), true)
    for y <- 0 until height do
      grid.update(Point(0, y), true)
      grid.update(Point(width - 1, y), true)

    // recurse, start with open space within walls
    def randomSplit(interval: Interval): Int =
      Random.nextInt(interval.size - 2) + 1

    def randomHoleHorizontal(grid: Grid[Boolean], interval: Interval, y: Int): Unit =
      val xSplit = Random.nextInt(interval.size)
      grid.update(Point(interval.min + xSplit, y), false)

    def randomHoleVertical(grid: Grid[Boolean], interval: Interval, x: Int): Unit =
      val ySplit = Random.nextInt(interval.size)
      grid.update(Point(x, interval.min + ySplit), false)

    def subMaze(grid: Grid[Boolean], openWidth: Interval, openHeight: Interval): Unit =
    // if open space is (either) less wide/high than 3: don't divide but return
    // divide both x and y randomly in half, make sure no half is less wide than 1
    // so take random of (1 to width - 2), this nr is the wall where counting starts at 0
    // make random holes in all 4 new walls
    // recurse for the 4 new area's
      if openWidth.size >= 3 && openHeight.size >= 3 then
        val widthSplit = randomSplit(openWidth)
        val heightSplit = randomSplit(openHeight)
        drawVerticalWall(grid, openWidth.min + widthSplit, openHeight)
        drawHorizontalWall(grid, openHeight.min + heightSplit, openWidth)

        val heightTop = Interval(openHeight.min, openHeight.min + heightSplit - 1)
        val heightBottom = Interval(openHeight.min + heightSplit + 1, openHeight.max)
        val widthLeft = Interval(openWidth.min, openWidth.min + widthSplit - 1)
        val widthRight = Interval(openWidth.min + widthSplit + 1, openWidth.max)

        randomHoleVertical(grid, heightTop, openWidth.min + widthSplit)
        randomHoleVertical(grid, heightBottom, openWidth.min + widthSplit)
        randomHoleHorizontal(grid, widthLeft, openHeight.min + heightSplit)
        randomHoleHorizontal(grid, widthRight, openHeight.min + heightSplit)

        subMaze(grid, widthLeft, heightTop)
        subMaze(grid, widthLeft, heightBottom)
        subMaze(grid, widthRight, heightTop)
        subMaze(grid, widthRight, heightBottom)

    val openWidth = Interval(1, width - 2)
    val openHeight = Interval(1, height - 2)
    subMaze(grid, openWidth, openHeight)

    // make sure start and end are open points
    val start = Point(1, 0)
    val end = Point(width - 2, height - 1)
    grid.update(start, false)
    grid.update(end, false)

    (grid, start, end)
  }
}

trait IGrid[T: ClassTag] extends WeightedGraph[Point, T] {
  def filterValid(pts: Seq[Point]): Seq[Point]

  def allPoints: Seq[Point]

  def width: Int

  def height: Int

  def value(point: Point): T
}

/**
 * Implements WeightedGraph where cost(a,b) is simply value(b)
 *
 * @param grid main array contains the rows, subArrays the columns.
 * @param getNeighbours returns all non-blocked neighbours for a point. Default is all horizontal/vertical ones.
 * @tparam T
 */
case class Grid[T: ClassTag](grid: Array[Array[T]], getNeighbours: (Grid[T], Point) => Seq[Point] = Grid.directNeighbours) extends IGrid[T] {
  override def width: Int = if grid.isEmpty then 0 else grid.head.length

  override def height: Int = grid.length

  override def value(point: Point): T = grid(point.y)(point.x)

  override def cost(from: Point, to: Point): T = grid(to.y)(to.x)

  override def neighbours(vertex: Point): Seq[Point] = getNeighbours(this, vertex)

  def update(point: Point, value: T): Unit = grid(point.y)(point.x) = value

  def rows: List[List[T]] = grid.map(_.toList).toList

  def columns: List[List[T]] =
    (0 until width).map { col =>
      val cols = grid.map{ row => row(col) }
      cols.toList
    }.toList

  override def allPoints: Seq[Point] =
    for {
      y <- grid.indices
      x <- grid.head.indices
    } yield Point(x, y)

  def allPointsLazy: LazyList[Point] =
    for {
      y <- grid.indices.to(LazyList)
      x <- grid.head.indices.to(LazyList)
    } yield Point(x, y)

  override def filterValid(pts: Seq[Point]): Seq[Point] =
    pts.filter(p => p.x >= 0 && p.x < grid.head.length && p.y >= 0 && p.y < grid.length)

  def forall(pred: T => Boolean): Boolean = {
    grid.forall(_.forall(pred))
  }

  def count(pred: T => Boolean): Int = {
    allPoints.count { p => pred(value(p)) }
  }

  def findPoint(pred: T => Boolean): Option[Point] = {
    allPointsLazy.find(p => pred(value(p)))
  }

  override def equals(o2: Any): Boolean = {
    if !o2.isInstanceOf[Grid[?]] then false else {
      val grid2 = o2.asInstanceOf[Grid[T]]
      width == grid2.width && height == grid2.height &&
        allPoints.forall { pt => value(pt) == grid2.value(pt) }
    }
  }
}

/**
 * Implements WeightedGraph where cost(a,b) is simply value(b)
 */
class MapGrid[T: ClassTag](grid: mutable.Map[Point, T], defaultValue: T, getNeighbours: (MapGrid[T], Point) => Seq[Point] = Grid.directNeighbours) extends IGrid[T] {

  override def width: Int = {
    val allX = allPoints.map(_.x)
    allX.max - allX.min + 1
  }

  override def height: Int = {
    val allY = allPoints.map(_.y)
    allY.max - allY.min + 1
  }
  
  override def value(point: Point): T = grid.getOrElse(point, defaultValue)

  override def cost(from: Point, to: Point): T = grid.getOrElse(to, defaultValue)

  override def neighbours(vertex: Point): Seq[Point] = getNeighbours(this, vertex)

  def update(point: Point, value: T): Unit = grid.update(point, value)

  override def allPoints: Seq[Point] =
    grid.keys.toSeq

  def allPointsLazy: LazyList[Point] =
    LazyList.from(grid.keys)

  override def filterValid(pts: Seq[Point]): Seq[Point] = pts

  def forall(pred: T => Boolean): Boolean = {
    allPoints.forall { k => pred(value(k)) }
  }

  def count(pred: T => Boolean): Int = {
    allPoints.count { p => pred(value(p)) }
  }

  def findPoint(pred: T => Boolean): Option[Point] = {
    allPointsLazy.find(p => pred(value(p)))
  }
}
