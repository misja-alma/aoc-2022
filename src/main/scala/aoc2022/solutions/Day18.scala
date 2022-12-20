package aoc2022.solutions

import aoc2022.utils.*
import scala.collection.mutable.ListBuffer

object Day18 {
  val sc = scannerFromResource("/day18.txt")
  val lines = scannerToLines(sc)

  val points = lines.map {
    case s"$x,$y,$z" => Point3D(x.toInt, y.toInt, z.toInt + 1) // shift all 1 up to eliminate 0
  }

  val pointSet = points.toSet
  val sortByX = points.sortBy(_.x).toArray
  val sortByY = points.sortBy(_.y).toArray
  val sortByZ = points.sortBy(_.z).toArray
  
  def sidesUncovered(pt: Point3D): Int = {
    val covered = pt.neighbours.count(pointSet)
    6 - covered
  }

  @main
  def day18Part1 = printSolution {
    val uncovered = points.map(sidesUncovered)
    uncovered.sum 
  }

  @main
  def day18Part2 = printSolution {

    val minX = sortByX.head.x - 1
    val maxX = sortByX.last.x + 1
    val minY = sortByY.head.y - 1
    val maxY = sortByY.last.y + 1
    val minZ = sortByZ.head.z - 1
    val maxZ = sortByZ.last.z + 1

    def isValid(pt: Point3D): Boolean = {
      pt.x >= minX && pt.x <= maxX &&
        pt.y >= minY && pt.y <= maxY &&
        pt.z >= minZ && pt.z <= maxZ
    }

    def validNeighbours(pt: Point3D): Seq[Point3D] = {
      pt.neighbours.filter(isValid)
    }

    // fill with water:
    // make 3d grid where value can be 0 (empty) 1 (point) 2 (water)
    // make queue of water points
    // for each of those:
    // add to grid
    // add all reachable dry pts to queue
    // repeat until queue is empty
    // then for each pt, do similar as 1, but look in grid and for neighbours that are water. return those (and not 6-0

    val grid3D = Array.fill(maxX + 1)(Array.fill(maxY + 1)(Array.fill(maxZ + 1)(0)))
    for pt <- points do
      grid3D(pt.x)(pt.y)(pt.z) = 1

    // make water pts
    val edgePts = ListBuffer[Point3D]()
    for x <- 0 to maxX do
      for y <- 0 to maxY do
        edgePts.append(Point3D(x, y, 0))
        edgePts.append(Point3D(x, y, maxZ))
      for z <- 0 to maxZ do
        edgePts.append(Point3D(x, 0, z))
        edgePts.append(Point3D(x, maxY, z))

    for y <- 0 to maxY do
      for z <- 0 to maxZ do
        edgePts.append(Point3D(0, y, z))
        edgePts.append(Point3D(maxX, y, z))

    val waterQueue = scala.collection.mutable.Queue.from(edgePts)
    val visited = scala.collection.mutable.Set.from(edgePts)
    while waterQueue.nonEmpty do {
      val nextWater = waterQueue.dequeue()
      grid3D(nextWater.x)(nextWater.y)(nextWater.z) = 2

      val unvisited = validNeighbours(nextWater).filterNot(visited)
      val empty = unvisited.filter { ep =>
        grid3D(ep.x)(ep.y)(ep.z) == 0
      }
      visited.addAll(empty)
      waterQueue.enqueueAll(empty)
    }

    // grid is now filled with water
    def wetSides(pt: Point3D): Int = {
      validNeighbours(pt).count { p =>
        grid3D(p.x)(p.y)(p.z) == 2
      }
    }

    val wet = points.map(wetSides)
    wet.sum
  }
}
