package fr.xebia.adventofcode2022

object Day08 extends AdventOfCode {

  override def fileName: String = "aoc8.txt"

  def count(data: Array[Array[Int]]): Int = {
    var counter = 0
    for (x <- data.indices) {
      for (y <- data.indices) {
        val tree = data(y)(x)
        if (
          !data(y).slice(0, x).exists(_ >= tree) |
            !data(y).slice(x + 1, data.length).exists(_ >= tree) |
            !data.transpose.apply(x).slice(0, y).exists(_ >= tree) |
            !data.transpose.apply(x).slice(y + 1, data.length).exists(_ >= tree)
        ) {
          counter += 1
        }
      }
    }
    counter
  }

  def computeSize(arr: Array[Int], tree: Int): Int = {
    val fullSize = arr.length
    val size = arr.takeWhile(_ < tree).length

    if (size == fullSize) size
    else size + 1
  }

  def count2(data: Array[Array[Int]]): Int = {
    var maxValue = 0
    for (x <- data.indices) {
      for (y <- data.indices) {
        val tree = data(y)(x)
        val left = computeSize(data(y).slice(0, x).reverse, tree)
        val right = computeSize(data(y).slice(x + 1, data.length), tree)
        val top = computeSize(data.transpose.apply(x).slice(0, y).reverse, tree)
        val bottom = computeSize(data.transpose.apply(x).slice(y + 1, data.length), tree)

        val value = left * right * top * bottom

        if(value > maxValue) {
          maxValue = value
        }
      }
    }
    maxValue
  }

  override def execute(): (Int, Int) = {
    val data: Array[Array[Int]] = input.map(_.split("").map(_.toInt))

    // Star 1
    val star1 = count(data)

    // Star 2
    val star2 = count2(data)

    (star1, star2)
  }
}
