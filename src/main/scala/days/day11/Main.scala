package days.day11

import utils.InputFileReader

import scala.annotation.tailrec

@main
def main(args: String*): Unit =
  val solution = partOneSolution()
  println(solution)
  
def partOneSolution(): Int =
  val lines = InputFileReader.getLinesFromFile("./src/main/scala/days/day11/input.txt").toList
  val expandedGalaxyMap = expandGalaxies(lines)
  val galaxies = getGalaxyCoordinates(expandedGalaxyMap)
  val galaxyPairs = galaxies.combinations(2).toList
  val shortestPaths = galaxyPairs.map(pair => calculateManhattanDistance(pair(0), pair(1)))
  val solution = shortestPaths.sum
  solution

def expandGalaxies(galaxyInput: List[String]): List[String] =
  def transposeGalaxyInput(input: List[String]): List[String] =
    input.map(_.toList).transpose.map(_.mkString)
  @tailrec
  def expandGalaxiesByRow(galaxyInput: List[String], acc: List[String] = List.empty): List[String] =
    def isEmptyLine(line: String) = !line.contains('#')
    def generateEmptyLine(lineLength: Int) = "." * lineLength

    galaxyInput match
      case Nil => acc.reverse
      case x :: xs =>
        if isEmptyLine(x) then
          val updatedAcc = generateEmptyLine(x.length) :: x :: acc
          expandGalaxiesByRow(xs, updatedAcc)
        else
          val updatedAcc = x :: acc
          expandGalaxiesByRow(xs, updatedAcc)

  val transposedGalaxies = transposeGalaxyInput(galaxyInput)
  val expandedTransposedGalaxies = expandGalaxiesByRow(transposedGalaxies)
  val expandedGalaxiesByColumn = transposeGalaxyInput(expandedTransposedGalaxies)
  val fullyExpandedGalaxies = expandGalaxiesByRow(expandedGalaxiesByColumn)
  fullyExpandedGalaxies

def getGalaxyCoordinates(galaxyMap: List[String]): List[(Int, Int)] =
  def galaxyRegex = "(#)".r
  @tailrec
  def scanGalaxiesByLine(galaxyLines: List[String], currentRow: Int = 0, acc: List[(Int, Int)] = List.empty): List[(Int, Int)] =
    galaxyLines match
      case Nil => acc.reverse
      case x :: xs =>
        val galaxyMatches = galaxyRegex.findAllMatchIn(x)
        val updatedAcc = galaxyMatches.map(m => (m.start, currentRow)).toList ::: acc
        scanGalaxiesByLine(xs, currentRow + 1, updatedAcc)
  scanGalaxiesByLine(galaxyMap)

def calculateManhattanDistance(p1: (Int, Int), p2: (Int, Int)): Int =
  math.abs(p1._1 - p2._1) + math.abs(p1._2 - p2._2)
