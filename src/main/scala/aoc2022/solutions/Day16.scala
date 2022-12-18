package aoc2022.solutions

import aoc2022.utils.*
import aoc2022.utils.Search.Path

object Day16 {
  val sc = scannerFromResource("/test.txt")
  val lines = scannerToLines(sc)

  val nodes = lines.map { line =>
    val node = line.drop("Valve ".length).take(2)
    val rateEnd = line.indexOf(";")
    val rate = line.substring("Valve ZN has flow rate=".length, rateEnd);
    val restStart = line.substring(rateEnd)
    val toStart = if restStart.contains("valves") then
      restStart.drop("; tunnels lead to valves ".length)
    else
      restStart.drop("; tunnels lead to valve ".length)
    val tos = toStart.split(", ")
    (node, rate.toInt, tos)
  }

  val neighboursMap = nodes.map { case (n, _, tos) => n -> tos }.toMap
  val ratesMap = nodes.map { case (n, rate, _) => n -> rate }.toMap
  val maxFlow = ratesMap.values.sum
  val allNodes = nodes.map(_._1).toList

  @main
  def day16Part1 = printSolution {
    case class GameState(nodesOpen: Set[String], position: String, flow: Int, turn: Int) {
      lazy val id = nodesOpen.mkString + position + "-" + turn

      override def equals(obj: Any): Boolean = id.equals(obj.asInstanceOf[GameState].id)

      override def hashCode(): Int = id.hashCode()
    }

    val startState = GameState(Set(), "AA", 0, 1)

    val visited = scala.collection.mutable.Set[GameState]()
    visited.add(startState)

    def handleNextTurn(gs: GameState): GameState = {
      val newFlow = gs.nodesOpen.map { nd => ratesMap(nd) }.sum
      gs.copy(
        flow = newFlow,
        turn = gs.turn + 1
      )
    }

    def openValve(gs: GameState): GameState = {
      val withFlow = handleNextTurn(gs)
      withFlow.copy(
        nodesOpen = withFlow.nodesOpen + withFlow.position,
        flow = withFlow.flow + ratesMap(withFlow.position)
      )
    }

    def moveTo(gs: GameState, nb: String): GameState = {
      val withFlow = handleNextTurn(gs)
      withFlow.copy(
        position = nb
      )
    }

    case class Game() extends Graph[GameState, Int] {
      override def neighbours(vertex: GameState): Seq[GameState] = {
        val meaningfulValves = if !vertex.nodesOpen.contains(vertex.position) && ratesMap(vertex.position) > 0
        then Seq(openValve(vertex))
        else Seq()

        val meaningfulNeighbours = neighboursMap(vertex.position).map { nb => moveTo(vertex, nb) }
        val meaningfulMoves = (meaningfulValves ++ meaningfulNeighbours).filterNot(visited)
        // assume nbrs with higher flow come first
        visited.addAll(meaningfulMoves)

        if meaningfulMoves.nonEmpty then meaningfulMoves else Seq(handleNextTurn(vertex))
      }

      override def value(edge: GameState): Int = edge.flow
    }

    class GameStateOrdering extends Ordering[Path[GameState]] {
      def compare(p1: Path[GameState], p2: Path[GameState]): Int = {

        def utilValue(p: Path[GameState]): Int = {
          val remainingPotential = (30 - p.endPoint.turn) * maxFlow
          p.total + remainingPotential
        }

        if p1.endPoint.turn == p2.endPoint.turn &&
          p1.endPoint.position == p2.endPoint.position &&
          p1.endPoint.nodesOpen == p2.endPoint.nodesOpen then

          p1.total.compareTo(p2.total)
        else
          val res = utilValue(p1).compareTo(utilValue(p2))
          if res == 0 then p1.endPoint.flow.compareTo(p2.endPoint.flow) else res
      }
    }

    val path = Search.findCheapestPath(new Game, startState, p => p.turn == 30, new GameStateOrdering()).get
    val solution = path.reverseSteps.map(_.flow).sum

    solution
  } // 1376

  @main
  def day16Part2 = printSolution {


    case class GameState(nodesOpen: Set[String], positionMe: String, positionElefant: String, flow: Int, turn: Int) {
      lazy val bothIds = List(positionMe, positionElefant).sorted.mkString
      lazy val id = nodesOpen.mkString + "-" + bothIds + "-" + turn

      override def equals(obj: Any): Boolean = id.equals(obj.asInstanceOf[GameState].id)

      override def hashCode(): Int = id.hashCode()
    }

    val startState = GameState(Set(), "AA", "AA", 0, 1)

    val visited = scala.collection.mutable.Set[GameState]()
    visited.add(startState)


    def handleNextTurn(gs: GameState): GameState = {
      val newFlow = gs.nodesOpen.map { nd => ratesMap(nd) }.sum
      gs.copy(
        flow = newFlow,
        turn = gs.turn + 1
      )
    }

    def openValve(gs: GameState, positionAt: String): GameState = {
      val withFlow = handleNextTurn(gs)
      withFlow.copy(
        nodesOpen = withFlow.nodesOpen + positionAt,
        flow = withFlow.flow + ratesMap(positionAt)
      )
    }

    def openBothValves(gs: GameState): GameState = {
      val withFlow = handleNextTurn(gs)
      withFlow.copy(
        nodesOpen = withFlow.nodesOpen + gs.positionMe + gs.positionElefant,
        flow = withFlow.flow + ratesMap(gs.positionMe) + ratesMap(gs.positionElefant)
      )
    }

    def moveTo(gs: GameState, me: String, elefant: String): GameState = {
      val withFlow = handleNextTurn(gs)
      withFlow.copy(
        positionMe = me,
        positionElefant = elefant
      )
    }

    var maxTurn = 0

    case class Game() extends Graph[GameState, Int] {
      override def neighbours(edge: GameState): Seq[GameState] = {
        if edge.turn > maxTurn then
          maxTurn = edge.turn
          println("New max turn: " + maxTurn)


        val goodMoves = if (edge.flow == maxFlow) then {
          println("Max flow reached! " + edge)

          Seq[GameState]()
        } else {

          val meaningfulValvesMe = if !edge.nodesOpen.contains(edge.positionMe) && ratesMap(edge.positionMe) > 0
          then Seq(openValve(edge, edge.positionMe))
          else Seq()
          val meaningfulValvesElefant = if !edge.nodesOpen.contains(edge.positionElefant) && ratesMap(edge.positionElefant) > 0
          then Seq(openValve(edge, edge.positionElefant))
          else Seq()

          val moveCombinations =
            for {
              me <- neighboursMap(edge.positionMe)
              elefant <- neighboursMap(edge.positionElefant)
              newState = moveTo(edge, me, elefant)
              if !visited(newState)
            } yield newState

          val moveMeValveCombinations =
            for {
              me <- neighboursMap(edge.positionMe)
              elefant <- meaningfulValvesElefant
              newState = elefant.copy(positionMe = me)
              if !visited(newState)
            } yield newState

          val moveElefantValveCombinations =
            for {
              me <- meaningfulValvesMe
              elefant <- neighboursMap(edge.positionElefant)
              newState = me.copy(positionElefant = elefant)
              if !visited(newState)
            } yield newState

          val valveValveCombinations =
            for {
              _ <- meaningfulValvesMe
              _ <- meaningfulValvesElefant
              if edge.positionMe != edge.positionElefant
              newState = openBothValves(edge)
              if !visited(newState)
            } yield newState


          val meaningfulMoves = (moveCombinations ++
            moveMeValveCombinations ++
            moveElefantValveCombinations ++
            valveValveCombinations).distinct

          // assume nbrs with higher flow come first
          visited.addAll(meaningfulMoves)

          meaningfulMoves.toSeq
        }

        if goodMoves.nonEmpty then goodMoves else Seq(handleNextTurn(edge))
      }

      override def value(edge: GameState): Int = edge.flow
    }

    class GameStateOrdering extends Ordering[Path[GameState]] {
      def compare(p1: Path[GameState], p2: Path[GameState]): Int = {

        def utilValue(p: Path[GameState]): Int = {
          // max remainingpotential:
          // take current flow and assume each turn you can open 2 for 23
          val rp = (p.endPoint.turn + 1 to 26).scanLeft(p.endPoint.flow) {
            case (totalFlow, _) => Math.min(maxFlow, totalFlow + 26)
          }.tail.sum

          val remainingPotential = rp //(26 - p.endPoint.turn) * maxFlow
          p.total + remainingPotential
        }

        if p1.endPoint.turn == p2.endPoint.turn &&
          p1.endPoint.bothIds == p2.endPoint.bothIds &&
          p1.endPoint.nodesOpen == p2.endPoint.nodesOpen then

          p1.total.compareTo(p2.total)
        else
          val res = utilValue(p1).compareTo(utilValue(p2))
          if res == 0 then p1.endPoint.flow.compareTo(p2.endPoint.flow) else res
      }
    }

    val gs1 = GameState(Set(), "AA", "BB", 1, 1)
    val gs2 = GameState(Set(), "BB", "AA", 1, 1)
    val s = Set(gs1)
    println("-- " + s.contains(gs2))


    val path = Search.findCheapestPathGreedy(new Game, startState, p => (p.turn == 26 || p.flow == maxFlow), new GameStateOrdering()).get
    path.reverseSteps.reverse.foreach {
      println
    }
    val solution = path.reverseSteps.map(_.flow).sum + (26 - path.endPoint.turn) * maxFlow

    solution

  } //1933


  @main
  def day16Part2Proper = printSolution {
    
    val directConnections = neighboursMap.toList.flatMap { case (node, neighbours) =>
      neighbours.map { n => (node -> n, 1) }
    }
    val allShortestPaths: Map[(String, String), Int] = Search.findAllShortestPaths(allNodes, directConnections)
    val nodesWithRates = allNodes.filter(ratesMap(_) > 0)

    case class WalkState(goal: String, stepsLeft: Int) {
      override def toString: String = s"$stepsLeft towards $goal"
    }

    val ME = 0
    val ELEFANT = 1
    case class GameState(nodesOpen: Set[String], positions: List[WalkState], flow: Int, turn: Int) {
      lazy val bothIds = positions.toSet.mkString
      lazy val id = nodesOpen.mkString + "-" + bothIds + "-" + turn

      override def equals(obj: Any): Boolean = id.equals(obj.asInstanceOf[GameState].id)

      override def hashCode(): Int = id.hashCode()
    }

    val startState = GameState(Set(), List(WalkState("AA", 0), WalkState("AA", 0)), 0, 1)

    val visited = scala.collection.mutable.Set[GameState]()
    visited.add(startState)

    def handleNextTurn(gs: GameState): GameState = {
      val newFlow = gs.nodesOpen.map { nd => ratesMap(nd) }.sum
      gs.copy(
        flow = newFlow,
        turn = gs.turn + 1
      )
    }

    def openValve(gs: GameState, positionAt: String): GameState = {
      gs.copy(
        nodesOpen = gs.nodesOpen + positionAt,
        flow = gs.flow + ratesMap(positionAt)
      )
    }

    def startMovingTo(gs: GameState, who: Int, goal: String): GameState = {
      val shortestPath = allShortestPaths((gs.positions(who).goal, goal))
      gs.copy(
        positions = gs.positions.updated(who, WalkState(goal, shortestPath - 1)) // we take 1 step already
      )
    }

    def stepFurther(gs: GameState, who: Int): GameState = {
      val current = gs.positions(who)
      gs.copy(
        positions = gs.positions.updated(who, current.copy(stepsLeft = current.stepsLeft - 1))
      )
    }

    def combine(gsMe: GameState, gsElefant: GameState): GameState = {
      val allOpen = gsMe.nodesOpen ++ gsElefant.nodesOpen
      GameState(
        nodesOpen = allOpen,
        positions = List(gsMe.positions(ME), gsElefant.positions(ELEFANT)),
        flow = allOpen.map(ratesMap).sum,
        turn = gsMe.turn
      )
    }

    var maxTurn = 0

    case class Game() extends Graph[GameState, Int] {
      override def neighbours(vertex: GameState): Seq[GameState] = {
        if vertex.turn > maxTurn then
          maxTurn = vertex.turn
          println("New max turn: " + maxTurn)


        val goodMoves = if (vertex.flow == maxFlow) then {
          println("Max flow reached! " + vertex)

          Seq[GameState]()
        } else {

          def allClosedValves(): Seq[String] =
            nodesWithRates.filterNot(vertex.nodesOpen)

          def meaningfulValveOpens(who: Int): Seq[GameState] = {
            val position = vertex.positions(who)
            if position.stepsLeft == 0 &&
              !vertex.nodesOpen.contains(position.goal) &&
              ratesMap(position.goal) > 0
            then Seq(openValve(vertex, position.goal))
            else Seq()
          }

          def meaningfulStartMovings(who: Int): Seq[GameState] = {
            val position = vertex.positions(who)
            if position.stepsLeft == 0 then
              allClosedValves().filterNot(_ == position.goal).map { cv => startMovingTo(vertex, who, cv) }
            else Seq()
          }

          def meaningfulMoves(who: Int): Seq[GameState] = {
            val position = vertex.positions(who)
            if position.stepsLeft == 0 then Seq()
            else Seq(stepFurther(vertex, who))
          }


          // make list of meaningful actions (as gamestates) for both players
          // then combine all of them; remove duplicate valve openings (take arbitrary one) and duplicate movings(?)
          // apply current move stuff
          // and filter already visited
          // give gamestate a list of WalkingStates with named indices for me, elefant, so we need only 1 update method

          val myValveOpens = meaningfulValveOpens(ME)
          val forMe = meaningfulMoves(ME) ++ meaningfulStartMovings(ME) ++ myValveOpens
          val elefantValveOpens =
            if vertex.positions(ME).goal == vertex.positions(ELEFANT).goal && myValveOpens.nonEmpty then Seq()
            else meaningfulValveOpens(ELEFANT)
          val forElefant = meaningfulMoves(ELEFANT) ++
            meaningfulStartMovings(ELEFANT) ++
            elefantValveOpens

          val allMoves = (for {
            mine <- forMe
            elefants <- forElefant
            combined = combine(mine, elefants)
          } yield handleNextTurn(combined)).distinct.filterNot(visited)

          // assume nbrs with higher flow come first
          visited.addAll(allMoves)

          allMoves
        }

        if goodMoves.nonEmpty then goodMoves else Seq(handleNextTurn(vertex))
      }

      override def value(edge: GameState): Int = edge.flow
    }

    class GameStateOrdering extends Ordering[Path[GameState]] {
      def compare(p1: Path[GameState], p2: Path[GameState]): Int = {

        def utilValue(p: Path[GameState]): Int = {
          val remainingPotential = (p.endPoint.turn + 1 to 26).scanLeft(p.endPoint.flow) {
            case (totalFlow, _) => Math.min(maxFlow, totalFlow + 26)
          }.tail.sum

          p.total + remainingPotential
        }

        if p1.endPoint.turn == p2.endPoint.turn &&
          p1.endPoint.bothIds == p2.endPoint.bothIds &&
          p1.endPoint.nodesOpen == p2.endPoint.nodesOpen then

          p1.total.compareTo(p2.total)
        else
          val res = utilValue(p1).compareTo(utilValue(p2))
          if res == 0 then p1.endPoint.flow.compareTo(p2.endPoint.flow) else res
      }
    }

    val path = Search.findCheapestPath(new Game, startState, p => (p.turn == 26 || p.flow == maxFlow), new GameStateOrdering()).get
    path.reverseSteps.reverse.foreach {
      println
    }
    val solution = path.reverseSteps.map(_.flow).sum + (26 - path.endPoint.turn) * maxFlow

    solution
  }
}

