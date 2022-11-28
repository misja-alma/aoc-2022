import java.io.InputStream
import java.util.Scanner
import scala.collection.mutable.ArrayBuffer

package object aoc2022 {
  def scannerFromResource(resourcePath: String): Scanner = {
    val istream: InputStream = getClass.getResourceAsStream(resourcePath)
    new Scanner(istream, "UTF-8")
  }

  def scannerToLines(sc: Scanner): Seq[String] = {
    val lineReader = sc.useDelimiter("\n")
    val result = ArrayBuffer[String]()
    while (lineReader.hasNext) result.append(lineReader.next())
    result.toSeq
  }
}
