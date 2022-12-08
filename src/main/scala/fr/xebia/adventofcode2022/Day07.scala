package fr.xebia.adventofcode2022

import scala.util.Try

object Day07 extends AdventOfCode {

  override def fileName: String = "aoc7.txt"

  case class File(name: String, size: Int)
  case class Directory(dirs: Map[String, Directory], files: Array[File])

  def getSizeOfDirectory(dir: Directory): Int = {
    val subDirs = dir.dirs.values
    dir.files.map(_.size).sum + (if(subDirs.isEmpty) 0 else subDirs.map(getSizeOfDirectory).sum)
  }

  override def execute(): (Int, Int) = {

    // Star 1
    val star1 = 0

    // Star 2
    val star2 = 0

    (star1, star2)
  }
}
