package fr.xebia.adventofcode2022

import scala.math.sqrt
import scala.math.pow

object Day09 extends AdventOfCode {

  override def fileName: String = "aoc9.txt"

  case class Movement(direction: String, length: Int)

  case class Rope(head: Position, tail: Position) {
    def move(direction: String): Rope = {
      val newHead = head.move(direction)
      val newTail = if (computeDistance(newHead, tail) < 1.5) tail
      else {
        if(newHead.x == tail.x) direction match {
          case "U" => Position(newHead.x, newHead.y - 1)
          case "D" => Position(newHead.x, newHead.y + 1)
        }
        else if(newHead.y == tail.y) direction match {
          case "R" => Position(newHead.x - 1, newHead.y)
          case "L" => Position(newHead.x + 1, newHead.y)
        }
        else direction match {
          case "U" => Position(newHead.x, newHead.y - 1)
          case "D" => Position(newHead.x, newHead.y + 1)
          case "R" => Position(newHead.x - 1, newHead.y)
          case "L" => Position(newHead.x + 1, newHead.y)
        }
      }
      Rope(newHead, newTail)
    }
  }

  case class RopeAndCounter(rope: Rope, counter: List[Position]) {
    def moveAndCount(mov: Movement): RopeAndCounter = {
      (1 to mov.length).foldLeft(this) { (x, _) =>
        val newRope = x.rope.move(mov.direction)
        val newCounter = x.counter :+ newRope.tail
        RopeAndCounter(newRope, newCounter)
      }
    }
  }

  def computeDistance(head: Position, tail: Position): Double = {
    sqrt(pow(head.x - tail.x, 2) + pow(head.y - tail.y, 2))
  }

  case class Position(x: Int, y: Int) {
    def move(direction: String): Position = direction match {
      case "U" => Position(x, y + 1)
      case "D" => Position(x, y - 1)
      case "R" => Position(x + 1, y)
      case "L" => Position(x - 1, y)
    }
  }

  override def execute(): (Int, Int) = {

    val smallData = Array(
      Movement("R", 4),
      Movement("U", 4),
      Movement("L", 3),
      Movement("D", 1),
      Movement("R", 4),
      Movement("D", 1),
      Movement("L", 5),
      Movement("R", 2)
    )

    val data = input.map(_.split(" ")).map(x => Movement(x.head, x(1).toInt))
    val initRope = Rope(Position(0, 0), Position(0, 0))
    val initRopeAndCounter = RopeAndCounter(initRope, List(initRope.tail))

    val result = data.foldLeft(initRopeAndCounter)(_.moveAndCount(_))

    //result.counter.foreach(println)
    // Star 1
    val star1 = result.counter.distinct.length

    // Star 2
    val star2 = 0

    (star1, star2)
  }
}
