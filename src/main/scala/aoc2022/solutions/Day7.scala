package aoc2022.solutions

import aoc2022.utils.*

object Day7 {
  val sc = scannerFromResource("/day7.txt")
  val lines = scannerToLines(sc)

  trait Output
  case class CdRelative(target: String) extends Output
  case class CdRoot() extends Output
  case class CdUp() extends Output
  case class Ls() extends Output
  case class File(name: String, size: Long) extends Output
  case class Directory(name: String) extends Output

  val cdroot = """\$ cd /$""".r
  val cdup = """\$ cd \\.\\.$""".r
  val cdrel= """\$ cd ([a-zA-Z\.]+)$""".r
  val ls = """\$ ls$""".r
  val file = """(\d+) ([a-zA-Z\.]+)$""".r
  val dir =  """dir ([a-zA-Z\.]+)$""".r

  def parseOutput(s: String): Output = s.trim() match {
    case cdrel(target) => if (target == "..") CdUp() else CdRelative(target)
    case cdup() => CdUp()
    case cdroot() => CdRoot()
    case ls() => Ls()
    case file(size, name) => File(name, size.toLong)
    case dir(name) => Directory(name)
    case _ => sys.error(s"Cant parse: $s")
  }

  case class DirGraph(name: String, size: Long, var children: Seq[DirGraph], parent: Option[DirGraph], isDir: Boolean)

  def remove(ds: Seq[DirGraph], d: Option[DirGraph]): Seq[DirGraph] = {
    if (d.isEmpty) ds else {
      val before = ds.takeWhile(_ != d.get)
      before ++ Seq(d.get) ++ ds.drop(before.size + 1)
    }
  }

  def findRootDir(graph: DirGraph): DirGraph =
    if (graph.isDir && graph.parent.isEmpty) graph else findRootDir(graph.parent.get)


  @main
  def day7Part1 = {
    val outputs = lines.map(parseOutput)

    // State is current node in DirGraph
    // Fold over lines
    val rootNode = DirGraph("/", 0, Seq(), None, true)

    val finalGraph = outputs.foldLeft(rootNode) {
        case (current, CdRelative(target)) =>
          current.children.find(c => c.isDir && c.name == target).get

        case (current, CdUp()) =>
          current.parent.get

        case (current, CdRoot()) =>
          findRootDir(current)

        case (current, Ls()) =>
          current

        case (current, File(name, size)) =>
          val child = DirGraph(name, size, Seq(), Some(current), false)
          current.children = child +: current.children
          current

        case (current, Directory(name)) =>
          val child = DirGraph(name, 0, Seq(), Some(current), true)
          current.children = child +: current.children
          current
    }

    val graphRoot = findRootDir(finalGraph)

    def addSizes(graph: DirGraph): DirGraph = {
      val subDirs = graph.children.filter(_.isDir).map(addSizes)
      val sizedGraph = graph.copy(children = graph.children.filterNot(_.isDir) ++ subDirs)
      val dirSize = sizedGraph.children.map(_.size).sum
      sizedGraph.copy(size = dirSize)
    }

    val rootWithSizes = addSizes(graphRoot)

    def upToMax(graph: DirGraph, maxSize: Long): Long = {
      // don't count root dir
      val own = if (graph.parent.isEmpty || graph.size > maxSize) 0 else graph.size
      val subDirs = graph.children.filter(_.isDir)
      own + subDirs.map(upToMax(_, maxSize)).sum
    }


    val solution = upToMax(rootWithSizes, 100000)
    println("Solution: " + solution)

    val currentFree = 70000000 - rootWithSizes.size
    val tobeFreed = 30000000 - currentFree

    def allDirChildren(g: DirGraph): Seq[DirGraph] = {
      g +: g.children.filter(_.isDir).flatMap(allDirChildren)
    }
    val allDirs = allDirChildren(rootWithSizes)
    val allSizes = allDirs.map(_.size)
    val smallest = allSizes.filter(_ >= tobeFreed)
    val solution2 = smallest.min // .minBy on graph throws Stackoverflow ???
    println(solution2)
  }

  @main
  def day7Part2 = {


  }
}
