package aoc2022.solutions

import aoc2022.utils.*

object Day15 {
  val sc = scannerFromResource("/day15.txt")
  val lines = scannerToLines(sc)

  val data = lines.map { line =>
    val (sensor, beacon) = line match {
      case s"Sensor at x=$sx, y=$sy: closest beacon is at x=$bx, y=$by" =>
        (Point(sx.toInt, sy.toInt), Point(bx.toInt, by.toInt))
      case _ => sys.error("Can't parse: " + line)
    }
    (sensor, beacon)
  }
  val (_, beacons) = data.unzip
  val beaconSet = beacons.toSet

  def reachableIntervalOnLine(y: Int, sensor: Point, beacon: Point): Option[Interval] = {
    val sensorReach = Math.abs(Point.manhattanDistance(sensor, beacon))
    val leftOverReach = sensorReach - Math.abs(y - sensor.y)
    if leftOverReach < 0 then None else Some(Interval(sensor.x - leftOverReach, sensor.x + leftOverReach))
  }


  @main
  def day15Part1 = printSolution {
    val checkY = 2000000
    val intervals = data.flatMap { case (sensor, beacon) =>
      reachableIntervalOnLine(checkY, sensor, beacon)
    } // Note list can be empty
    val start = intervals.map(_.min).min
    val end = intervals.map(_.max).max
    val solution = (start to end).count { x =>
      val pt = Point(x, checkY)
      intervals.exists(_.contains(x)) && !beaconSet.contains(pt)
    }

    solution
  }

  @main
  def day15Part2 = printSolution {
    val solution = LazyList.from(0 to 4000000).flatMap { checkY =>

      val intervals = data.flatMap { case (sensor, beacon) =>
        reachableIntervalOnLine(checkY, sensor, beacon)
      }
      val sortedByX = intervals.sortBy(_.min)
      val lastCoveredX = sortedByX.tail.foldLeft(sortedByX.head.max) { case (maxCoveredX, interval) =>
        if interval.min <= maxCoveredX + 1 then
          Math.max(maxCoveredX, interval.max)
        else
          maxCoveredX
      }

      if lastCoveredX < 4000000 then
        val solutionX = lastCoveredX + 1
        Some(Point(solutionX, checkY))
      else None
    }.head

    solution.x * 4000000L + solution.y
  }
}