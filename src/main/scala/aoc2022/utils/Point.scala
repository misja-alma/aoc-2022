package aoc2022.utils

case class Point(x: Int, y: Int) {
  override def toString: String = s"($x,$y)"
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
  def +(pos2: Point3D): Point3D = Point3D(x + pos2.x, y + pos2.y, z + pos2.z)

  def -(pos2: Point3D): Point3D = Point3D(x - pos2.x, y - pos2.y, z - pos2.z)

  override def toString: String = s"($x,$y,$z)"
}
