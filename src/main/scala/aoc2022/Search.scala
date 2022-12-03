package aoc2022

import scala.collection.mutable

object Search {
  case class Path(reverseSteps: Seq[Point], total: Int, point: Point) {
    override def toString: String = s"Total $total: " + reverseSteps.reverse.mkString(" -> ")
  }

  object CheapestPathFirstOrdering extends Ordering[Path] {
    def compare(p1: Path, p2: Path): Int = {
      p2.total.compareTo(p1.total)
    }
  }

  /**
   * Returns the cheapest path from startPoint to endPoint.
   * Cost is the sum the values of the points in the grid, not counting the startPoint.
   */
  def findCheapestPath(grid: Grid[Int], startPoint: Point, endPoint: Point): Option[Path] = {
    val visited = mutable.Map[Point, Int]()
    visited.put(startPoint, 0)
    val startPath = Path(Seq(startPoint), 0, startPoint)
    // reverse ordering for Scala's pq; without special ordering it would put the largest in the head but we want the cheapest path
    val queue = mutable.PriorityQueue[Path]()(CheapestPathFirstOrdering)
    queue.enqueue(startPath)
    var best = if startPoint == endPoint then Some(startPath) else Option.empty[Path]

    while (best.isEmpty && queue.nonEmpty) {
      val p = queue.dequeue()
      if (p.point == endPoint) {
        if best.isEmpty || p.total < best.get.total then best = Some(p)
      } else {
        visited.put(p.point, p.total)
        grid.neighbours(p.point).foreach { nb =>
          val newTotal = p.total + grid.value(nb)
          val alreadyVisited2 = visited.getOrElse(nb, Integer.MAX_VALUE)
          // for A* this check makes a huge difference .. Maybe because pq becomes slow as well with many nodes?
          if (alreadyVisited2 > newTotal) {
            visited.put(nb, newTotal)
            queue.enqueue(Path(nb +: p.reverseSteps, newTotal, nb))
          }
        }
      }
    }

    best
  }

  /**
   * Returns the shortest path from startPoint to a point where stopCondition = true
   * Only travels over points that have value False (i.e. not blocked) in the grid
   */
  def findShortestPath(grid: Grid[Boolean], startPoint: Point, stopCondition: Point => Boolean): Option[Seq[Point]] = {
    val startPath = Seq(startPoint)

    val queue = mutable.Queue.from[Seq[Point]](Seq(startPath))
    val visited = mutable.Set[Point]()
    visited.add(startPoint)
    var best = Option.empty[Seq[Point]]

    while (queue.nonEmpty && best.isEmpty) {
       val path = queue.dequeue()
       val point = path.head
       if stopCondition(point) then best = Some(path)
       else
         visited.add(point)
         val nextCandidates = grid
           .neighbours(point)
           .filterNot(visited)
           .filterNot(grid.value)  // Only consider points with value false
           .map { pt =>
             pt +: path
           }
         queue.enqueueAll(nextCandidates)
    }

    best.map(_.reverse)
  }
}
