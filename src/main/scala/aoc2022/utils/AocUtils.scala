package aoc2022.utils

import java.io.InputStream
import java.util.Scanner
import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import scala.reflect.ClassTag
import scala.util.Random

def scannerFromResource(resourcePath: String): Scanner = {
  val stream = getClass.getResourceAsStream(resourcePath)
  new Scanner(stream, "UTF-8")
}

def scannerToLines(sc: Scanner): Seq[String] = {
  val lineReader = sc.useDelimiter("\n")
  val result = ArrayBuffer[String]()
  while (lineReader.hasNext) result.append(lineReader.next())
  result.toSeq
}

def printSolution(solver: => Any): Unit = {
  val start = System.currentTimeMillis()
  val solution = solver
  val time = System.currentTimeMillis() - start
  println("Solution: " + solution)
  println(s"Solving took $time milliseconds")
}

/**
 *
 * @param seq
 * @param predicate
 * @param includeSeparators if true, includes the separator that ended each Seq. Note: this could be more efficient
 * @tparam T
 * @return
 */
def split[T](seq: Seq[T], predicate: T => Boolean, includeSeparators: Boolean = false): Seq[Seq[T]] = {
    val (before, after) = seq.span(predicate andThen (!_))
    (before, after) match {
      case (Nil, Nil) => Nil
      case (Nil, h +: hs) =>
        if includeSeparators then
          Seq(Seq(h)) ++ split(hs, predicate, includeSeparators)
        else
          split(hs, predicate, includeSeparators)
      case (_, Nil) => Seq(before)
      case (_, h +: hs) =>
        if includeSeparators then
          Seq(before :+ h) ++ split(hs, predicate, includeSeparators)
        else
          Seq(before) ++ split(hs, predicate, includeSeparators)
    }
}

trait Graph[V, E] {
  def neighbours(vertex: V): Seq[V]
}

trait WeightedGraph[V, E] extends Graph[V, E] {
  def cost(from: V, to: V): E
}
