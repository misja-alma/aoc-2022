package aoc2022.solutions

import aoc2022.utils.*
import aoc2022.utils.Search.Path

object Day16 {
  val sc = scannerFromResource("/day16.txt")
  val lines = scannerToLines(sc)

  // Valve ZN has flow rate=0; tunnels lead to valves SD, ZV
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

    var maxTurn = 0

    case class Game() extends Graph[GameState, Int] {
      override def neighbours(edge: GameState): Seq[GameState] = {
        if edge.turn > maxTurn then
          maxTurn = edge.turn
          println("New max turn: " + maxTurn)


        val meaningfulValves = if !edge.nodesOpen.contains(edge.position) && ratesMap(edge.position) > 0
        then Seq(openValve(edge))
        else Seq()

        val meaningfulNeighbours = neighboursMap(edge.position).map { nb => moveTo(edge, nb) }.filterNot(visited)
        // assume nbrs with higher flow come first
        visited.addAll(meaningfulNeighbours)

        val meaningfulMoves = meaningfulValves ++ meaningfulNeighbours

        if meaningfulMoves.nonEmpty then meaningfulMoves else Seq(handleNextTurn(edge))
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
          utilValue(p1).compareTo(utilValue(p2))
      }
    }
    
      val path = Search.findCheapestPath(new Game, startState, p => p.turn == 30, new GameStateOrdering()).get
      path.reverseSteps.reverse.foreach { println }
      val solution = path.reverseSteps.map(_.flow).sum

      solution
    }

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
          println ("Max flow reached! " + edge)

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
          val remainingPotential = (26 - p.endPoint.turn) * maxFlow
          p.total + remainingPotential
        }

        if p1.endPoint.turn == p2.endPoint.turn &&
          p1.endPoint.bothIds == p2.endPoint.bothIds &&
          p1.endPoint.nodesOpen == p2.endPoint.nodesOpen then

          p1.total.compareTo(p2.total)
        else
          utilValue(p1).compareTo(utilValue(p2))
      }
    }

    val gs1 = GameState(Set(), "AA", "BB", 1, 1)
    val gs2 = GameState(Set(), "BB", "AA", 1, 1)
    val s = Set(gs1)
    println ("-- " + s.contains(gs2))


    val path = Search.findCheapestPath(new Game, startState, p => (p.turn == 26 || p.flow == maxFlow), new GameStateOrdering()).get
    path.reverseSteps.reverse.foreach {
      println
    }
    val solution = path.reverseSteps.map(_.flow).sum + (26 - path.endPoint.turn) * maxFlow

    solution

    }
}