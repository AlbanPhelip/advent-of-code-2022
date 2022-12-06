package fr.xebia.adventofcode2022

object Day05 extends AdventOfCode {

  case class Line(numberOfPackToMove: Int, oldStack: Int, newStack: Int)

  override def fileName: String = "aoc5.txt"

  val cheetInput = Map(
    1 -> Array("D", "H", "R", "Z", "S", "P", "W", "Q"),
    2 -> Array("F", "H", "Q", "W", "R", "B", "V"),
    3 -> Array("H", "S", "V", "C"),
    4 -> Array("G", "F", "H"),
    5 -> Array("Z", "B", "J", "G", "P"),
    6 -> Array("L", "F", "W", "H", "J", "T", "Q"),
    7 -> Array("N", "J", "V", "L", "D", "W", "T", "Z"),
    8 -> Array("F", "H", "G", "J", "C", "Z", "T", "D"),
    9 -> Array("H", "B", "M", "V", "P", "W")
  )

  def parseLine(line: String): Line = {
    val pattern = "move ([0-9]+) from ([0-9]) to ([0-9])".r
    val pattern(x, y, z) = line
    Line(x.toInt, y.toInt, z.toInt)
  }

  def moveStackV1(stacks: Map[Int, Array[String]], line: Line): Map[Int, Array[String]] = {
    (1 to line.numberOfPackToMove).foldLeft(stacks) { case (stack, _) =>
      stack + (line.oldStack -> stack(line.oldStack).tail) + (line.newStack -> (Array(stack(line.oldStack).head) ++ stack(line.newStack)))
    }
  }

  def moveStackV2(stacks: Map[Int, Array[String]], line: Line): Map[Int, Array[String]] = {
    val packsToMove = stacks(line.oldStack).slice(0, line.numberOfPackToMove)
    stacks + (line.newStack -> (packsToMove ++ stacks(line.newStack))) + (line.oldStack -> stacks(line.oldStack).slice(line.numberOfPackToMove, stacks(line.oldStack).length))
  }

  def compute(moveStack: (Map[Int, Array[String]], Line) => Map[Int, Array[String]]): String = {
    input
      .filter(_.startsWith("move"))
      .map(parseLine)
      .foldLeft(cheetInput)(moveStack)
      .toList
      .sortBy(_._1)
      .map(_._2.head)
      .mkString("")
  }

  override def execute(): (String, String) = {
    // Star 1
    val star1 = compute(moveStackV1)

    // Star 2
    val star2 = compute(moveStackV2)

    (star1, star2)
  }
}
