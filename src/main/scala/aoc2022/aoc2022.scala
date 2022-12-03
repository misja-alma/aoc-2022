package aoc2022

import java.io.InputStream
import java.util.Scanner
import scala.collection.mutable.ArrayBuffer
import java.io.InputStream
import java.util.Scanner
import scala.collection.mutable.ArrayBuffer
import scala.reflect.ClassTag
import scala.util.Random


def scannerFromResource(resourcePath: String): Scanner = {
  val istream: InputStream = getClass.getResourceAsStream(resourcePath)
  new Scanner(istream, "UTF-8")
}

def scannerToLines(sc: Scanner): Seq[String] = {
  val lineReader = sc.useDelimiter("\n")
  val result = ArrayBuffer[String]()
  while (lineReader.hasNext) result.append(lineReader.next())
  result.toSeq
}


case class Point(x: Int, y: Int) {
  override def toString: String = s"($x,$y)"
}

object Point {
  def apply(s: String): Point =
    s match
      case s"$x,$y" => Point(x.toInt, y.toInt)
      case _ => sys.error("Cant parse Point: " + s)

  def manhattanDistance(p1: Point, p2: Point): Int = Math.abs(p1.x - p2.x) + Math.abs(p1.y - p2.y)
}

case class Pos3D(x: Int, y: Int, z: Int) {
  def +(pos2: Pos3D): Pos3D = Pos3D(x + pos2.x, y + pos2.y, z + pos2.z)

  def -(pos2: Pos3D): Pos3D = Pos3D(x - pos2.x, y - pos2.y, z - pos2.z)

  override def toString: String = s"($x,$y,$z)"
}

trait Graph[E, V] {
  def neighbours(edge: E): Seq[E]

  def value(edge: E): V
}

object Grid {
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

  def printBooleanGrid(grid: Grid[Boolean], cutEmptySpace: Boolean = true): Unit = {
    val (maxX, maxY) = if cutEmptySpace then
      val allFilled = grid.allPoints.filter { p => grid.value(p) }
      (allFilled.maxBy(_.x).x, allFilled.maxBy(_.y).y)
    else
      (grid.width - 1, grid.height - 1)

    for row <- 0 to maxY do
      for col <- 0 to maxX do
        if grid.value(Point(col, row)) then print('#') else print('.')
      println()
  }

  def printBooleanGridWithPath(grid: Grid[Boolean], path: Seq[Point], cutEmptySpace: Boolean = true): Unit = {
    val (maxX, maxY) = if cutEmptySpace then
      val allFilled = grid.allPoints.filter { p => grid.value(p) }
      (allFilled.maxBy(_.x).x, allFilled.maxBy(_.y).y)
    else
      (grid.width - 1, grid.height - 1)

    val points = path.toSet

    for row <- 0 to maxY do
      for col <- 0 to maxX do
        val pt = Point(col, row)
        val toPrint = if points.contains(pt) then 'O' else if grid.value(pt) then '#' else '.'
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

  def printGridWithPath(grid: Grid[?], path: Seq[Point], pathMarker: String = "O"): Unit = {
    val points = path.toSet
    val maxX = grid.width
    val maxY = grid.height
    for y <- 0 until maxY do
      for x <- 0 until maxX do
        val pt = Point(x, y)
        val toPrint = if points.contains(pt) then pathMarker else grid.value(pt)
        print(toPrint)
      println()
  }

  /**
   * Creates a random maze, returns the grid, start, end
   */
  def createMaze(width: Int, height: Int): (Grid[Boolean], Point, Point) = {
    // fill maze with empty space
    val grid = Grid.withDimensions(width, height, initialValue = false)

    def drawVerticalWall(grid: Grid[Boolean], x: Int, yInterval: Interval): Unit =
      for y <- yInterval.min to yInterval.max do
        grid.update(Point(x, y), true)

    def drawHorizontalWall(grid: Grid[Boolean], y: Int, xInterval: Interval): Unit =
      for x <- xInterval.min to xInterval.max do
        grid.update (Point(x, y), true)

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

/**
 *
 * @param grid main array contains the rows, subArrays the columns.
 * @tparam T
 */
class Grid[T](grid: Array[Array[T]]) extends Graph[Point, T] {
  def width: Int = if grid.isEmpty then 0 else grid.head.length

  def height: Int = grid.length

  def value(point: Point): T = grid(point.y)(point.x)

  def update(point: Point, value: T): Unit = grid(point.y)(point.x) = value

  def allPoints: Seq[Point] =
    for {
      y <- grid.indices
      x <- grid.head.indices
    } yield Point(x, y)

  def allPointsLazy: LazyList[Point] =
    for {
      y <- grid.indices.to(LazyList)
      x <- grid.head.indices.to(LazyList)
    } yield Point(x, y)

  def filterValid(pts: Seq[Point]): Seq[Point] =
    pts.filter(p => p.x >= 0 && p.x < grid.head.length && p.y >= 0 && p.y < grid.length)

  def neighbours(point: Point): Seq[Point] = {
    val raw = Seq(Point(point.x - 1, point.y), Point(point.x, point.y - 1), Point(point.x + 1, point.y), Point(point.x, point.y + 1))
    filterValid(raw)
  }

  def allNeighbours(point: Point): Seq[Point] = {
    val raw = for {
      px <- point.x - 1 to point.x + 1
      py <- point.y - 1 to point.y + 1
    } yield Point(px, py)

    filterValid(raw)
  }

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
    if !o2.isInstanceOf[Grid[T]] then false else {
      val grid2 = o2.asInstanceOf[Grid[T]]
      width == grid2.width && height == grid2.height &&
        allPoints.forall { pt => value(pt) == grid2.value(pt) }
    }
  }
}

def iterate[T](start: T)(f: T => T): LazyList[T] = {
  def doIterate(newStart: T): LazyList[T] =
    newStart #:: doIterate(f(newStart))

  doIterate(start)
}

type FrequencyMap[T] = Map[T, Long]

def FrequencyMap[T](): FrequencyMap[T] = Map[T, Long]()

implicit class FrequencyMapOps[T](map: FrequencyMap[T]) {
  def addCount(el: T, count: Long): FrequencyMap[T] = {
    val newCount = count + map.getOrElse(el, 0L)
    map.updated(el, newCount)
  }
}

/**
 * Inclusive on both sides
 */
case class Interval(min: Int, max: Int) {
  def contains(x: Int): Boolean = x >= min && x <= max

  def size: Int = max - min + 1

  def intersect(i2: Interval): Option[Interval] = {
    if min <= i2.min && max >= i2.min then
      Some(Interval(i2.min, Math.min(max, i2.max)))
    else if min <= i2.max && min >= i2.min then
      Some(Interval(min, Math.min(max, i2.max)))
    else
      None
  }
}

