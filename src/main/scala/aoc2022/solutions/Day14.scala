package aoc2022.solutions

import aoc2022.utils.*

object Day14 {
  val sc = scannerFromResource("/day14.txt")
  val lines = scannerToLines(sc)

  val pts = lines.flatMap { line =>
    val pts = line.split(" -> ").map { p =>
      val cs = p.split(',').map(_.toInt)
      Point(cs.head, cs.last)
    }

    val allPts = pts.sliding(2, 1).flatMap { ps =>
      val start = ps.head
      val end = ps.last
      if start.x != end.x then
        val startX = Math.min(start.x, end.x)
        val endX = Math.max(start.x, end.x)
        (startX to endX).map { x => Point(x, start.y) }
      else
        val startY = Math.min(start.y, end.y)
        val endY = Math.max(start.y, end.y)
        (startY to endY).map { y => Point(start.x, y) }
    }
    allPts
  }
  
  val bottom = pts.maxBy(_.y)
  val bottomLine = (0 to 1000).map { x => Point(x, bottom.y + 2)}

  def dropSandUntil(endCondition: Point => Boolean): Int = {
    val grid = Grid.infiniteFromPoints(pts ++ bottomLine, pointValue = true, defaultValue = false)
    
    var nrSand = 0
    var endConditionReached = false
    while !endConditionReached do {
      nrSand = nrSand + 1

      var sandPt = Point(500, 0)
      var inRest = false
      while !inRest do {
        var nextPt = Point(sandPt.x, sandPt.y + 1)
        if !grid.value(nextPt) then
          sandPt = nextPt
        else
          nextPt = Point(sandPt.x - 1, sandPt.y + 1)
          if !grid.value(nextPt) then
            sandPt = nextPt
          else
            nextPt = Point(sandPt.x + 1, sandPt.y + 1)
            if !grid.value(nextPt) then
              sandPt = nextPt
            else
              grid.update(sandPt, true)
              inRest = true
      }

      endConditionReached = endCondition(sandPt)
    }

    nrSand
  }


  @main
  def day14Part1 = {
    val solution = dropSandUntil(pt => pt.y == bottom.y - 1) - 1
    println ("Solution: " + solution)
  }

  @main
  def day14Part2 = {
    val solution = dropSandUntil(pt => pt.x == 500 && pt.y == 0)
    println("Solution: " + solution)
  }
}
