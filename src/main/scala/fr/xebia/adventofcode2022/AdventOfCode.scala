package fr.xebia.adventofcode2022

import scala.io.Source

abstract class AdventOfCode {

  def fileName: String

  def execute(): (Int, Int)

  val input: Array[String] = {
    val file = Source.fromFile(s"/Users/EAPP09211/Documents/Perso/AdventOfCode/$fileName")
    val input = file.getLines.toArray
    file.close()
    input
  }

  def main(args: Array[String]): Unit = {
    val (star1, star2) = execute()
    println("****************")
    println(s"Result for star 1 is $star1")
    println(s"Result for star 2 is $star2")
    println("****************")
    println()
  }
}
