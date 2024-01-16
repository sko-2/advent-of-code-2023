package days.day11

import utils.InputFileReader

import scala.annotation.tailrec

@main
def main(args: String*): Unit =
  val solution = partTwoSolution()
  println(solution)
  
def partOneSolution(): Long =
  solution(expandingDistance = 1)

def partTwoSolution(): Long =
  solution(expandingDistance = 999999)

def solution(expandingDistance: Int): Long =
  val lines = InputFileReader.getLinesFromFile("./src/main/scala/days/day11/input.txt").toList
  val expandedGalaxyMap = expandGalaxies(lines)
  val galaxies = getGalaxyCoordinates(expandedGalaxyMap, expandingDistance = expandingDistance)
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
    def generateExpandingLine(lineLength: Int) = "*" * lineLength

    galaxyInput match
      case Nil => acc.reverse
      case x :: xs =>
        if isEmptyLine(x) then
          val updatedAcc = generateExpandingLine(x.length) :: x :: acc
          expandGalaxiesByRow(xs, updatedAcc)
        else
          val updatedAcc = x :: acc
          expandGalaxiesByRow(xs, updatedAcc)

  val transposedGalaxies = transposeGalaxyInput(galaxyInput)
  val expandedTransposedGalaxies = expandGalaxiesByRow(transposedGalaxies)
  val expandedGalaxiesByColumn = transposeGalaxyInput(expandedTransposedGalaxies)
  val fullyExpandedGalaxies = expandGalaxiesByRow(expandedGalaxiesByColumn)
  fullyExpandedGalaxies

def getGalaxyCoordinates(galaxyMap: List[String], expandingDistance: Int = 999999): List[(Long, Long)] =
  def getGalaxyIndices(galaxyLine: String): List[Long] =
    @tailrec
    def parseGalaxyLine(galaxyInput: List[Char], columnStart: Long = 0, acc: List[Long] = List.empty): List[Long] =
      galaxyInput match
        case Nil => acc
        case x :: xs =>
          x match
            case '.' => parseGalaxyLine(xs, columnStart + 1, acc)
            case '*' => parseGalaxyLine(xs, columnStart + expandingDistance, acc)
            case '#' =>
              val updatedAcc = columnStart :: acc
              parseGalaxyLine(xs, columnStart + 1, updatedAcc)
            case _ => throw RuntimeException("Unexpected input.")

    parseGalaxyLine(galaxyLine.toList)
  @tailrec
  def scanGalaxiesByLine(galaxyLines: List[String], currentRow: Long = 0, acc: List[(Long, Long)] = List.empty): List[(Long, Long)] =
    def isExpandingRow(line: String): Boolean =
      line.head == '*'
    galaxyLines match
      case Nil => acc.reverse
      case x :: xs =>
        if isExpandingRow(x) then
          scanGalaxiesByLine(xs, currentRow + expandingDistance, acc)
        else
          val galaxyIndices = getGalaxyIndices(x)
          val updatedAcc = galaxyIndices.map(i => (i, currentRow)) ::: acc
          scanGalaxiesByLine(xs, currentRow + 1, updatedAcc)
  scanGalaxiesByLine(galaxyMap)

def calculateManhattanDistance(p1: (Long, Long), p2: (Long, Long)): Long =
  math.abs(p1._1 - p2._1) + math.abs(p1._2 - p2._2)
