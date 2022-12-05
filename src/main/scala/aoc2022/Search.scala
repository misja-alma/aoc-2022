package aoc2022

import scala.collection.mutable

object Search {
  case class Path[E](reverseSteps: Seq[E], total: Int, point: E) {
    override def toString: String = s"Total $total: " + reverseSteps.reverse.mkString(" -> ")
  }

  class CheapestPathFirstOrdering[E] extends Ordering[Path[E]] {
    def compare(p1: Path[E], p2: Path[E]): Int = {
      p2.total.compareTo(p1.total)
    }
  }

  /**
   * Returns the cheapest path from startPoint to endPoint.
   * Cost is the sum the values of the points in the grid, not counting the startPoint.
   */
  def findCheapestPath[E](grid: Graph[E, Int], startPoint: E, endPoint: E): Option[Path[E]] = {
    val visited = mutable.Map[E, Int]()
    visited.put(startPoint, 0)
    val startPath = Path[E](Seq(startPoint), 0, startPoint)
    // reverse ordering for Scala's pq; without special ordering it would put the largest in the head but we want the cheapest path
    val queue = mutable.PriorityQueue[Path[E]]()(new CheapestPathFirstOrdering[E]())
    queue.enqueue(startPath)
    var best = if startPoint == endPoint then Some(startPath) else Option.empty[Path[E]]

    while (best.isEmpty && queue.nonEmpty) {
      val p = queue.dequeue()
      // Note: p.point could have been visited already, but still this visit might bring a lower cost
      // this is because we add points to visited already when we're adding them as neighbours
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
            queue.enqueue(Path[E](nb +: p.reverseSteps, newTotal, nb))
          }
        }
      }
    }

    best
  }

  /**
   * Returns the shortest path from startPoint to a point where stopCondition = true
   * Only travels over points that have value False (i.e. not blocked) in the grid
   * NOTE: maybe better to let neighbours take this into account
   */
  def findShortestPath[E](grid: Graph[E, Boolean], startPoint: E, stopCondition: E => Boolean): Option[Seq[E]] = {
    val startPath = Seq(startPoint)

    val queue = mutable.Queue.from[Seq[E]](Seq(startPath))
    val visited = mutable.Set[E]()
    var best = Option.empty[Seq[E]]

    while (queue.nonEmpty && best.isEmpty) {
       val path = queue.dequeue()
       val point = path.head
       if stopCondition(point) then best = Some(path)
       else
         if !visited.contains(point) then 
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
