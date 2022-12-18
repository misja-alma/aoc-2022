package aoc2022.utils

import scala.annotation.targetName

case class Point(x: Int, y: Int) {
  import aoc2022.utils.Point._

  override def toString: String = s"($x,$y)"

  lazy val neighbours = Seq(left(this), right(this), up(this), down(this))
}

object Point {
  def apply(s: String): Point =
    s match
      case s"$x,$y" => Point(x.toInt, y.toInt)
      case _ => sys.error("Cant parse Point: " + s)

  def manhattanDistance(p1: Point, p2: Point): Int = Math.abs(p1.x - p2.x) + Math.abs(p1.y - p2.y)

  def up(p: Point): Point = p.copy(y = p.y - 1)
  def down(p: Point): Point = p.copy(y = p.y + 1)
  def left(p: Point): Point = p.copy(x = p.x - 1)
  def right(p: Point): Point = p.copy(x = p.x + 1)
}

case class Point3D(x: Int, y: Int, z: Int) {
  @targetName("add")
  def +(pos2: Point3D): Point3D = Point3D(x + pos2.x, y + pos2.y, z + pos2.z)

  @targetName("subtract")
  def -(pos2: Point3D): Point3D = Point3D(x - pos2.x, y - pos2.y, z - pos2.z)

  override def toString: String = s"($x,$y,$z)"

  lazy val neighbours: Seq[Point3D] = {
    val leftNeighbour = copy(x = x - 1)
    val rightNeighbour = copy(x = x + 1)
    val upNeighbour = copy(z = z - 1)
    val downNeighbour = copy(z = z + 1)
    val frontNeighbour = copy(y = y - 1)
    val backNeighbour = copy(y = y + 1)

    Seq(leftNeighbour, rightNeighbour, upNeighbour, downNeighbour, frontNeighbour, backNeighbour)
  }
}
