package aoc2022.solutions

import aoc2022.utils.*

object Day7 {
  val sc = scannerFromResource("/day7.txt")
  val lines = scannerToLines(sc)

  enum Output:
    case CdRelative(target: String) extends Output
    case CdRoot() extends Output
    case CdUp() extends Output
    case Ls() extends Output
    case File(name: String, size: Long) extends Output
    case Directory(name: String) extends Output

  import Output._

  val cdroot = """\$ cd /$""".r
  val cdup = """\$ cd \.\.$""".r  // NOTE: we need only single backslashes to escape because we are already within triple quotes
  val cdrel= """\$ cd ([a-zA-Z\.]+)$""".r
  val ls = """\$ ls$""".r
  val file = """(\d+) ([a-zA-Z\.]+)$""".r
  val dir =  """dir ([a-zA-Z\.]+)$""".r

  def parseOutput(s: String): Output = s.trim() match {
    case cdup() => CdUp()
    case cdrel(target) => CdRelative(target)
    case cdroot() => CdRoot()
    case ls() => Ls()
    case file(size, name) => File(name, size.toLong)
    case dir(name) => Directory(name)
    case _ => sys.error(s"Cant parse: $s")
  }

  // NOTE: Don't try to print this, it will throw a StackOverflow .. Reason is the cyclic dependency parent - child
  case class DirGraph(name: String, size: Long, var children: Seq[DirGraph], parent: Option[DirGraph], isDir: Boolean)

  def findRootDir(graph: DirGraph): DirGraph =
    if (graph.isDir && graph.parent.isEmpty) graph else findRootDir(graph.parent.get)

  val outputs = lines.map(parseOutput)

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

    case invalid => sys.error("Invalid output: " + invalid)
  }

  val graphRoot = findRootDir(finalGraph)

  def addSizes(graph: DirGraph): DirGraph = {
    val subDirs = graph.children.filter(_.isDir).map(addSizes)
    val sizedGraph = graph.copy(children = graph.children.filterNot(_.isDir) ++ subDirs)
    val dirSize = sizedGraph.children.map(_.size).sum
    sizedGraph.copy(size = dirSize)
  }

  val rootWithSizes = addSizes(graphRoot)

  @main
  def day7Part1 = printSolution {

    def sumSizesUptoMax(graph: DirGraph, maxSize: Long): Long = {
      // don't count root dir
      val own = if graph.parent.isEmpty || graph.size > maxSize then 0 else graph.size
      val subDirs = graph.children.filter(_.isDir)
      own + subDirs.map(sumSizesUptoMax(_, maxSize)).sum
    }

    val solution = sumSizesUptoMax(rootWithSizes, 100000)
    solution
  }

  @main
  def day7Part2 = printSolution {
    val currentFree = 70000000 - rootWithSizes.size
    val tobeFreed = 30000000 - currentFree

    def allDirChildren(g: DirGraph): Seq[DirGraph] = {
      g +: g.children.filter(_.isDir).flatMap(allDirChildren)
    }

    val allDirs = allDirChildren(rootWithSizes)
    val solution = allDirs.filter(_.size >= tobeFreed).minBy(_.size)
    solution.size
  }
}
