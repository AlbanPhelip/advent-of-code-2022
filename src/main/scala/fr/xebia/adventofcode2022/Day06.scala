package fr.xebia.adventofcode2022

object Day06 extends AdventOfCode {

  override def fileName: String = "aoc6.txt"

  def compute(size: Int): Int = {
    (0 to input.head.length - size)
      .map(i => input.head.slice(i, i + size))
      .map(_.mkString("").distinct.length)
      .zipWithIndex
      .filter(_._1 == size)
      .head
      ._2 + size
  }

  override def execute(): (Int, Int) = {
    // Star 1
    val star1 = compute(4)

    // Star 2
    val star2 = compute(14)

    (star1, star2)
  }
}
