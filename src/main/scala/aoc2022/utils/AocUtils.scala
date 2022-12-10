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

trait Graph[E, V] {
  def neighbours(edge: E): Seq[E]

  def value(edge: E): V
}
