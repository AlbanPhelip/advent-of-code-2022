package fr.xebia.adventofcode2022

object Day04 extends AdventOfCode {

  override def fileName: String = "aoc4.txt"

  val parsed: Array[((Int, Int), (Int, Int))] = input.
    map(_.split(",")).
    map(x => ((x(0).split("-")(0).toInt, x(0).split("-")(1).toInt), (x(1).split("-")(0).toInt, x(1).split("-")(1).toInt)))

  def fullOverlapCondition(x1: Int, x2: Int, y1: Int, y2: Int): Boolean = (x1 >= y1 & x2 <= y2) || (x1 <= y1 & x2 >= y2)

  def partialOverlapCondition(x1: Int, x2: Int, y1: Int, y2: Int): Boolean = (x1 >= y1 & x1 <= y2) || (x2 >= y1 & x2 <= y2)


  override def execute(): (Int, Int) = {
    // Star 1
    val star1 = parsed.count { case ((x1, x2), (y1, y2)) => fullOverlapCondition(x1, x2, y1, y2) }

    // Star 2
    val star2 = parsed.count { case ((x1, x2), (y1, y2)) => fullOverlapCondition(x1, x2, y1, y2) || partialOverlapCondition(x1, x2, y1, y2) }

    (star1, star2)
  }
}
