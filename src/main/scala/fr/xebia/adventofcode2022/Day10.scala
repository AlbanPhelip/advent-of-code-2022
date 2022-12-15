package fr.xebia.adventofcode2022

object Day10 extends AdventOfCode {

  override def fileName: String = "aoc10.txt"

  override def execute(): (Int, Int) = {
    val data = input
      .flatMap {
        case "noop" => Array(0)
        case line => Array(0, line.split(" ")(1).toInt)
      }
      .zipWithIndex
      .map { case (value, index) =>  (value, index + 1) }
      .map {
        case (_, 1) =>  (1, 1)
        case x => x
      }

    val result = data.map { case (value, index) => (index, value, data.slice(0, index).map(_._1).sum)}
    val fres = result.map { case (index, value, sum) => (index, value, sum, result.find(_._1 == index - 1).map(_._3).getOrElse(0)  * index)}

    // Star 1
    val star1 = List(20, 60, 100, 140, 180, 220).map(i => {
      fres.filter(_._1 == i).head._4
    }).sum

    // Star 2
    val star2 = 0

    (star1, star2)
  }
}
