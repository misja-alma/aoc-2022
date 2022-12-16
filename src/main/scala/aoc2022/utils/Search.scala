package aoc2022.utils

import scala.collection.mutable

object Search {

  case class Path[E](reverseSteps: Seq[E], total: Int, endPoint: E) {
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
   * Note: greedy algorithm; assumes that nodes that were visited already with lower total cost can always be preferred.
   */
  def findCheapestPathGreedy[V](grid: Graph[V, Int], startPoint: V, endCondition: V => Boolean,
                                ordering: Ordering[Path[V]] = new CheapestPathFirstOrdering[V]()): Option[Path[V]] = {
    val visited = mutable.Map[V, Int]()
    visited.put(startPoint, 0)
    val startPath = Path[V](Seq(startPoint), 0, startPoint)
    // reverse ordering for Scala's pq; without special ordering it would put the largest in the head but we want the cheapest path
    val queue = mutable.PriorityQueue[Path[V]]()(ordering)
    queue.enqueue(startPath)
    var best = if endCondition(startPoint) then Some(startPath) else Option.empty[Path[V]]

    while (best.isEmpty && queue.nonEmpty) {
      val p = queue.dequeue()
      // Note: p.point could have been visited already, but still this visit might bring a lower cost
      // this is because we add points to visited already when we're adding them as neighbours
      if (endCondition(p.endPoint)) {
        if best.isEmpty || p.total < best.get.total then best = Some(p)
      } else {
        visited.put(p.endPoint, p.total)
        grid.neighbours(p.endPoint).foreach { nb =>
          val newTotal = p.total + grid.value(nb)
          val alreadyVisited2 = visited.getOrElse(nb, Integer.MAX_VALUE)
          // for A* this check makes a huge difference .. Maybe because pq becomes slow as well with many nodes?
          if (alreadyVisited2 > newTotal) {
            visited.put(nb, newTotal)
            queue.enqueue(Path[V](nb +: p.reverseSteps, newTotal, nb))
          }
        }
      }
    }

    best
  }

  /**
   * Returns the cheapest path from startPoint to endPoint.
   * Cost is the sum the values of the points in the grid, not counting the startPoint.
   * Note: the graph.neighbours function needs to take care of the fact if it wants to skip any visited nodes
   */
  def findCheapestPath[V](grid: Graph[V, Int], startPoint: V, endCondition: V => Boolean,
                          ordering: Ordering[Path[V]] = new CheapestPathFirstOrdering[V]()): Option[Path[V]] = {
    val startPath = Path[V](Seq(startPoint), 0, startPoint)
    // reverse ordering for Scala's pq; without special ordering it would put the largest in the head but we want the cheapest path
    val queue = mutable.PriorityQueue[Path[V]]()(ordering)
    queue.enqueue(startPath)
    var best = if endCondition(startPoint) then Some(startPath) else Option.empty[Path[V]]

    while (best.isEmpty && queue.nonEmpty) {
      val p = queue.dequeue()
      // Note: p.point could have been visited already, but still this visit might bring a lower cost
      // this is because we add points to visited already when we're adding them as neighbours
      if (endCondition(p.endPoint)) {
        if best.isEmpty || p.total < best.get.total then best = Some(p)
      } else {
        grid.neighbours(p.endPoint).foreach { nb =>
          val newTotal = p.total + grid.value(nb)
          queue.enqueue(Path[V](nb +: p.reverseSteps, newTotal, nb))
        }
      }
    }

    best
  }

  /**
   * Returns the shortest path from startPoint to a point where stopCondition = true
   */
  def findShortestPath[V](grid: Graph[V, ?], startPoint: V, stopCondition: V => Boolean): Option[Seq[V]] = {
    val startPath = Seq(startPoint)

    val queue = mutable.Queue.from[Seq[V]](Seq(startPath))
    val visited = mutable.Set[V]()
    var best = Option.empty[Seq[V]]

    while (queue.nonEmpty && best.isEmpty) {
      val path = queue.dequeue()
      val point = path.head
      if stopCondition(point) then best = Some(path)
      else if !visited.contains(point) then
        visited.add(point)
        val nextCandidates = grid
          .neighbours(point)
          .filterNot(visited)
          .map { pt =>
            pt +: path
          }
        queue.enqueueAll(nextCandidates)
    }

    best.map(_.reverse)
  }

  /**
   * See https://en.wikipedia.org/wiki/Floyd%E2%80%93Warshall_algorithm
   *
   * let dist be a |V| × |V| array of minimum distances initialized to ∞ (infinity)
   * for each edge (u, v) do
   * dist[u][v] ← w(u, v)  // The weight of the edge (u, v): Note: should be >= 0
   * for each vertex v do
   *   dist[v][v] ← 0
   * for k from 1 to |V|
   *   for i from 1 to |V|
   *     for j from 1 to |V|
   *       if dist[i][j] > dist[i][k] + dist[k][j]
   *         dist[i][j] ← dist[i][k] + dist[k][j]
   *       end if
   */
  def findAllShortestPaths[V](vertices: List[V], weightedEdges: List[((V, V), Int)]): Map[(V, V), Int] = {
    val dist = Array.fill(vertices.length)(Array.fill(vertices.length)(Integer.MAX_VALUE))
    val vertexIndices = vertices.zipWithIndex.toMap
    weightedEdges.foreach { case ((from, to), weight) =>
      val iFrom = vertexIndices(from)
      val iTo = vertexIndices(to)
      dist(iFrom)(iTo) = weight
    }

    for v <- 0 until vertices.length do
      dist(v)(v) = 0

    for k <- 0 until vertices.length do
      for i <- 0 until vertices.length do
        for j <- 0 until vertices.length do
          if (dist(i)(k) != Integer.MAX_VALUE && dist(k)(j) != Integer.MAX_VALUE) &&
            (dist(i)(j) == Integer.MAX_VALUE || dist(i)(j) > dist(i)(k) + dist(k)(j)) then
            dist(i)(j) = dist(i)(k) + dist(k)(j)

    vertices.flatMap { v1 =>
      val distPairs = for {
        v2 <- vertices
        v1i = vertexIndices(v1)
        v2i = vertexIndices(v2)
        distanceForward = dist(v1i)(v2i)
        distanceBackward = dist(v2i)(v1i)
      } yield Seq(((v1, v2), distanceForward), ((v2, v1), distanceBackward))

      distPairs.flatten
    }.toMap
  }
}
