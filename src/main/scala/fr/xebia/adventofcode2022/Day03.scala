package fr.xebia.adventofcode2022

import scala.collection.MapView

object Day03 extends AdventOfCode {

  override def fileName: String = "aoc3.txt"

  def getLetter(str: String): String = {
    val list = str.split("")
    val (sack1, sack2) = list.splitAt(list.length / 2)
    sack1.filter(sack2.contains).head
  }

  def getCommonElements(l1: Array[String], l2: Array[String], l3: Array[String]): String = {
    l1.filter(l2.contains).filter(l3.contains).head
  }

  val letterValues: MapView[String, Int] = (('a' to 'z') ++ ('A' to 'Z')).map(_.toString).zipWithIndex.toMap.view.mapValues(_ + 1)

  override def execute(): (Int, Int) = {

    // Star 1
    val star1 = input.map(getLetter).map(letterValues(_)).sum

    // Star 2
    val star2 = input.
      zipWithIndex.
      map(x => (x._2 / 3, x._1)).
      groupBy(_._1).
      values.
      map(_.map(_._2).map(_.split(""))).
      map(x => getCommonElements(x(0), x(1), x(2))).
      map(letterValues(_)).
      sum

    (star1, star2)
  }
}
