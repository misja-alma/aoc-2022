package aoc2022.utils

type FrequencyMap[T] = Map[T, Long]

def FrequencyMap[T](): FrequencyMap[T] = Map[T, Long]()

implicit class FrequencyMapOps[T](map: FrequencyMap[T]) {
  def addCount(el: T, count: Long): FrequencyMap[T] = {
    val newCount = count + map.getOrElse(el, 0L)
    map.updated(el, newCount)
  }
}
