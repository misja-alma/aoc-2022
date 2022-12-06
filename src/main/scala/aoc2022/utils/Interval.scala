package aoc2022.utils

trait IInterval {
  def contains(x: Int): Boolean

  def contains(i2: IInterval): Boolean

  def size: Int

  def intersect(i2: IInterval): IInterval

  def union(i2: IInterval): IInterval

  def isEmpty: Boolean = size == 0

  def nonEmpty: Boolean = size > 0
}

case object EmptyInterval extends IInterval {

  override def contains(x: Int): Boolean = false

  override def contains(i2: IInterval): Boolean = false

  override def size: Int = 0

  override def intersect(i2: IInterval): IInterval = this

  override def union(i2: IInterval): IInterval = i2
}

/**
 * Inclusive on both sides
 */
case class Interval(min: Int, max: Int) extends IInterval {
  assert(min <= max, s"Interval.min $min > Interval.max $max")

  override def contains(x: Int): Boolean = x >= min && x <= max

  override def contains(i2: IInterval): Boolean =  i2 match {
    case EmptyInterval => true
    case Interval(min2, max2) => contains(min2) && contains(max2)
  }

  override def size: Int = max - min + 1

  override def intersect(i2: IInterval): IInterval = i2 match {
    case EmptyInterval => EmptyInterval

    case Interval(min2, max2) =>
      val (imin, imax) = (Math.max(min, min2), Math.min(max, max2))
      if imin <= imax then Interval(imin, imax) else EmptyInterval
  }

  override def union(i2: IInterval): IInterval = i2 match {
    case EmptyInterval => this

    case Interval(min2, max2) =>
      Interval(Math.min(min, min2), Math.max(max, max2))
  }
}

