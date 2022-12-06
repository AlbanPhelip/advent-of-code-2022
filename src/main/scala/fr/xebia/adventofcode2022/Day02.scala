package fr.xebia.adventofcode2022

object Day02 extends AdventOfCode {

  override def fileName: String = "aoc2.txt"

  def computeScore1(x: String, y: String): Int = {
    (x, y) match {
      case ("A", "X") => 3 + 1
      case ("A", "Y") => 6 + 2
      case ("A", "Z") => 0 + 3
      case ("B", "X") => 0 + 1
      case ("B", "Y") => 3 + 2
      case ("B", "Z") => 6 + 3
      case ("C", "X") => 6 + 1
      case ("C", "Y") => 0 + 2
      case ("C", "Z") => 3 + 3
    }
  }

  def computeScore2(x: String, y: String): Int = {
    (x, y) match {
      case ("A", "X") => 0 + 3
      case ("A", "Y") => 3 + 1
      case ("A", "Z") => 6 + 2
      case ("B", "X") => 0 + 1
      case ("B", "Y") => 3 + 2
      case ("B", "Z") => 6 + 3
      case ("C", "X") => 0 + 2
      case ("C", "Y") => 3 + 3
      case ("C", "Z") => 6 + 1
    }
  }

  override def execute(): (Int, Int) = {
    // Star 1
    val star1 = input.map(_.split(" ")).map(x => computeScore1(x(0), x(1))).sum

    // Star 2
    val star2 = input.map(_.split(" ")).map(x => computeScore2(x(0), x(1))).sum

    (star1, star2)
  }
}
