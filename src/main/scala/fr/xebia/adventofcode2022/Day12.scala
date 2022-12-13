package fr.xebia.adventofcode2022

import scala.math.abs

object Day12 extends AdventOfCode {

  override def fileName: String = "aoc12.txt"

  def letters(firstLetter: Char): Array[(Char, Char)] = Array(
    (firstLetter, 'D'),
    ('D', 'E'),
    ('E', 'H'),
    ('H', 'J'),
    ('J', 'I'),
    ('I', 'K'),
    ('K', 'M'),
    ('M', 'O'),
    ('O', 'P'),
    ('P', 'R'),
    ('R', 'S'),
    ('S', 'T'),
    ('T', 'U'),
    ('U', 'V'),
    ('V', 'W'),
    ('W', 'X'),
    ('X', 'Y'),
    ('Y', 'Z')
  )

  case class Point(x: Int, y: Int)

  def getCoordinate(letter: Char): Point = {
    val y = input.zipWithIndex.filter(_._1.contains(letter)).head._2
    val x = input(y).split("").zipWithIndex.filter(_._1.contains(letter)).head._2
    Point(x, y)
  }

  def computeManhattanDistance(p1: Point, p2: Point): Int = {
    abs(p1.x - p2.x) + abs(p1.y - p2.y)
  }

  def computePathLength(letters: Array[(Char, Char)]): Int = {
    letters.map { case (currentLetter, nextLetter) =>
      val currentLetterCoord = getCoordinate(currentLetter)
      val nextLetterCoord = getCoordinate(nextLetter)
      computeManhattanDistance(currentLetterCoord, nextLetterCoord)
    }.sum
  }

  override def execute(): (Int, Int) = {
    // Star 1
    val star1 = computePathLength(letters('A'))

    // Star 2
    val star2 = computePathLength(letters('B'))

    (star1, star2)
  }
}
