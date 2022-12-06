package fr.xebia.adventofcode2022

object Day01 extends AdventOfCode {

  override def fileName: String = "aoc1.txt"

  override def execute(): (Int, Int) = {
    val data = input.mkString(",")
      .split(",,")
      .map(_.split(",").map(_.toInt).sum)

    // Star 1
    val star1 = data.max

    // Star 2
    val star2 = data.sorted.reverse.take(3).sum

    (star1, star2)
  }
}
