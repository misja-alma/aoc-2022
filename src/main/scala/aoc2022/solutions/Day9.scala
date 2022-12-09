package aoc2022.solutions

import aoc2022.utils.*

import scala.collection.mutable.ListBuffer

object Day9 {
  val sc = scannerFromResource("/day9.txt")
  val lines = scannerToLines(sc)


  def follow(leader: Point, follower: Point): Point = {
    val xDif = leader.x - follower.x
    val yDif = leader.y - follower.y
    if Math.abs(xDif) <= 1 && Math.abs(yDif) <= 1
    then follower // no change needed if still connected
    else {
      val dx = Math.signum(xDif).toInt
      val dy = Math.signum(yDif).toInt
      follower.copy(x = follower.x + dx, y = follower.y + dy)
    }
  }

  case class State(leader: Point, followers: List[Point], visited: Set[Point])

  def move(state: State, direction: Point => Point, distance: Int): State = {
    (distance until 0 by -1).foldLeft(state) { case (state: State, _) =>
      val newLeader = direction(state.leader)
      followTheLeader(newLeader, state)
    }
  }

  def followTheLeader(leader: Point, state: State): State = {
    val newFollowers = state.followers.scanLeft(leader) { case (previous, follower) =>
      follow(previous, follower)
    }.tail
    val newVisited = state.visited + newFollowers.last
    State(leader, newFollowers, newVisited)
  }

  def traverseRoute(startState: State): State = {
    lines.foldLeft(startState) { case (state: State, line) =>
      line match {
        case s"L $s" =>
          move(state, Point.left, s.toInt)
        case s"R $s" =>
          move(state, Point.right, s.toInt)
        case s"U $s" =>
          move(state, Point.up, s.toInt)
        case s"D $s" =>
          move(state, Point.down, s.toInt)
        case _ => sys.error("Can parse line: " + line)
      }
    }
  }

  @main
  def day9Part1 = {
    val followers = List(Point(0, 0))
    val finalState = traverseRoute(State(Point(0, 0), followers, Set(Point(0, 0))))
    val solution = finalState.visited.size
    println ("Solution: " + solution)
  }

  @main
  def day9Part2 = {
    val followers = List.fill(9)(Point(0, 0))
    val finalState = traverseRoute(State(Point(0, 0), followers, Set(Point(0, 0))))
    val solution = finalState.visited.size
    println("Solution: " + solution)
  }
}
