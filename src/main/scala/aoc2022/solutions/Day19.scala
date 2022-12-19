package aoc2022.solutions

import aoc2022.utils.*
import Search.Path

object Day19 {
  val sc = scannerFromResource("/day19.txt")
  val lines = scannerToLines(sc)

  case class BluePrint(nr: Int, oreOre: Int, clayOre: Int, obsidianOre: Int, obsidianClay: Int, geodeOre: Int, geodeObsidian: Int)

  val blueprints = lines.map {
    case s"Blueprint $nr: Each ore robot costs $oreOre ore. Each clay robot costs $clayOre ore. Each obsidian robot costs $obsidianOre ore and $obsidianClay clay. Each geode robot costs $geodeOre ore and $geodeObsidian obsidian." =>
       BluePrint(nr.toInt, oreOre.toInt, clayOre.toInt, obsidianOre.toInt, obsidianClay.toInt, geodeOre.toInt, geodeObsidian.toInt)
  }

  case class GameState(oresOpen: Int, claysOpen: Int, obsidiansOpen: Int, geodesOpen: Int,
                       ores: Int, clays: Int, obsidian: Int, geodes: Int, minutes: Int)

  class GameStateOrdering extends Ordering[Path[GameState]] {
    def compare(p1: Path[GameState], p2: Path[GameState]): Int = {
      def utility(gs: GameState): Int = {
        gs.geodes + (24 - gs.minutes) * (gs.geodesOpen + 2)
      }

      val ep1 = p1.endPoint
      val ep2 = p2.endPoint
      val res = utility(ep1).compareTo(utility(ep2))
      if res == 0 then {
        val res2 = ep1.obsidiansOpen.compareTo(ep2.obsidiansOpen)
        if res2 == 0 then {
          val res3 = ep1.claysOpen.compareTo(ep2.claysOpen)
          if res3 == 0 then {
            val res4 = ep1.oresOpen.compareTo(ep2.oresOpen)
            if res4 == 0 then ep2.minutes.compareTo(ep1.minutes)
            else res4
          } else res3
        } else res2
      } else res
    }
  }

  var maxMinute = 0
  var visited = scala.collection.mutable.Set[GameState]()

  case class Game(bluePrint: BluePrint) extends WeightedGraph[GameState, Int] {
    def neighbours(vertex: GameState): Seq[GameState] = {
      if vertex.minutes > maxMinute then {
        maxMinute = vertex.minutes
        println(maxMinute)
      }

      val newOres = vertex.ores
      val newClays = vertex.clays
      val newObsidian = vertex.obsidian
      val newGeodes = vertex.geodes

       // calc all possible buys + the not buy
       // note we can max buy 1 at a time
       val noBuy = vertex.copy(ores = newOres, clays = newClays, obsidian = newObsidian, geodes = newGeodes, minutes = vertex.minutes + 1)
       val buyOres =  if (newOres >= bluePrint.oreOre) {
         val withBuy = vertex.copy(
           ores = newOres  - bluePrint.oreOre,
           oresOpen = vertex.oresOpen + 1,
           clays = newClays, obsidian = newObsidian, geodes = newGeodes, minutes = vertex.minutes + 1)
         Some(withBuy)
       } else None

      val buyClays = if (newOres >= bluePrint.clayOre) {
        val withBuy = vertex.copy(
          ores = newOres - bluePrint.clayOre,
          claysOpen = vertex.claysOpen + 1,
          clays = newClays, obsidian = newObsidian, geodes = newGeodes, minutes = vertex.minutes + 1)
        Some(withBuy)
      } else None

      val buyObsidians = if (newOres >= bluePrint.obsidianOre && newClays >= bluePrint.obsidianClay) {
        val withBuy = vertex.copy(
          ores = newOres - bluePrint.obsidianOre,
          clays = newClays - bluePrint.obsidianClay,
          obsidiansOpen = vertex.obsidiansOpen + 1,
          obsidian = newObsidian, geodes = newGeodes, minutes = vertex.minutes + 1)
        Some(withBuy)
      } else None

      val buyGeodes = if (newOres >= bluePrint.geodeOre && newObsidian >= bluePrint.geodeObsidian) {
        val withBuy = vertex.copy(
          ores = newOres - bluePrint.geodeOre,
          obsidian = newObsidian - bluePrint.geodeObsidian,
          geodesOpen = vertex.geodesOpen + 1,
          clays = newClays, geodes = newGeodes, minutes = vertex.minutes + 1)
        Some(withBuy)
      } else None

      val allOptions = noBuy +: Seq(buyOres, buyClays, buyObsidians, buyGeodes).flatten
      // collect money only afterwards
      val withMoney = allOptions.map { newGs =>
        newGs.copy (
          ores = newGs.ores + vertex.oresOpen,
          clays = newGs.clays + vertex.claysOpen,
          obsidian = newGs.obsidian + vertex.obsidiansOpen,
          geodes = newGs.geodes + vertex.geodesOpen,
        )
      }
      val filtered = withMoney.filterNot(visited)
      visited.addAll(filtered)
      filtered
    }

    def value(edge: GameState): Int = edge.geodes // TODO not used?

    def cost(from: GameState, to: GameState): Int = to.geodes - from.geodes
  }

  def maxNrGeodes(bluePrint: BluePrint): Int = {
    println ("---- Checking nr: " + bluePrint.nr)
    maxMinute = 0

    val startState = GameState(1, 0, 0, 0, 0, 0, 0, 0, 0)
    val game = Game(bluePrint)
    val ordering = new GameStateOrdering()
    visited.clear()
    visited.add(startState)

    val path = Search.findCheapestPath(game, startState, s => s.minutes == 24, ordering)
    path.get.total
  }

  @main
  def day19Part1 = printSolution {
     val solutions = blueprints.map { bp => (bp, maxNrGeodes(bp)) }
     val solution = solutions .map {
       case (bp, geodes) => bp.nr * geodes
     }.sum

     solution
  }

  @main
  def day19Part2 = printSolution {


  }
}
