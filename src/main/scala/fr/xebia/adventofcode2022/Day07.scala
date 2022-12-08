package fr.xebia.adventofcode2022

/**
 * This is entirely copy/paste from https://gist.github.com/franck-cussac/ffff26d6a34b99841a3f0a95b64205ac because flemme
 */
object Day07 extends AdventOfCode {

  override def fileName: String = "aoc7.txt"

  case class File(name: String, size: Int)
  case class Directory(name: String, files: Array[File], dirs: Array[Directory]) {
    def getSize: Int = {
      files.map(_.size).sum + dirs.map(_.getSize).sum
    }

    def flatDirectories: Array[Directory] = {
      dirs ++ dirs.flatMap(_.flatDirectories)
    }
  }
  case class RawCommand(command: String, pos: Int)
  case class Path(path: String)

  def getDirName(dir: String): String = {
    dir.split(' ').last
  }

  object File {
    def apply(file: String): File = {
      val list = file.split(' ')
      new File(list(1), list.head.toInt)
    }
  }

  def isDirectory(line: String): Boolean = line.startsWith("dir")
  def isCommand(line: String): Boolean = line.startsWith("$")
  def isCommandCD(line: String): Boolean = line.startsWith("$ cd")
  def isCommandCDDotDot(line: String): Boolean = line == "$ cd .."
  def isFile(line: String): Boolean = !isDirectory(line) && !isCommand(line)

  val refPath: Map[Path, RawCommand] = addPath(input)

  def addPath(lines: Array[String]): Map[Path, RawCommand] = {
    var path = ""
    lines.zipWithIndex.filter(x => isCommandCD(x._1)).map(x => {
      if (isCommandCDDotDot(x._1)) {
        path = path.split(' ').dropRight(1).mkString(" ")
      }
      else if (isCommandCD(x._1)) {
        path = s"$path ${getDirName(x._1)}"
      }
      (Path(path), RawCommand(x._1, x._2))
    }).filter(x => !isCommandCDDotDot(x._2.command)).toMap
  }

  def createDirectory(name: String, pos: Int, path: Path): Directory = {
    val elems = input.slice(pos + 2, input.length).takeWhile(x => !isCommand(x))

    val files = elems.filter(isFile).map(x => File(x))

    val directories = elems.filter(isDirectory).map(dir => {
      val dirName = getDirName(dir)
      val newPath = Path(s"${path.path} $dirName")
      val dirPos = refPath(newPath).pos
      createDirectory(dirName, dirPos, newPath)
    })
    Directory(name, files, directories)
  }

  override def execute(): (Int, Int) = {

    val root = createDirectory("/", 0, Path(" /"))

    // Star 1
    val directories = root.flatDirectories
    val directoriesSize = directories.map(_.getSize)
    val star1 = directoriesSize.filter(x => x <= 100000).sum

    // Star 2
    val totalDisk = 70000000
    val requiredFreeSpace = 30000000
    val spaceUsed = root.getSize
    val spaceToRelease = requiredFreeSpace - (totalDisk - spaceUsed)
    val star2 = directoriesSize.filter(x => x >= spaceToRelease).min

    (star1, star2)
  }
}
