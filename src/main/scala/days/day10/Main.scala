package days.day10

import days.day10.MapTile.*
import utils.InputFileReader

import scala.annotation.tailrec

@main
def main(args: String*): Unit =
  val result = partTwoSolution()
  println(result)

def partOneSolution(): Int =
  val pipePath = solvePipePath()
  val midPoint = (pipePath.length / 2) + (pipePath.length % 2)
  midPoint

def partTwoSolution(): Int =
  def triangleFormulaArea(points: List[(Int, Int)]): Int =
    def laceUpTerms(xs: List[Int], ys: List[Int]): List[Int] =
      xs.zip(ys.drop(1) :+ ys.head).map(x => x._1 * x._2)
    val xs = points.map(_._1)
    val ys = points.map(_._2)

    val positiveTerms = laceUpTerms(xs, ys)
    val negativeTerms = laceUpTerms(ys, xs)

    val combinedTerms = positiveTerms.zip(negativeTerms).map(x => x._1 - x._2).sum
    combinedTerms / 2

  def solveForInteriorPointsWithPickersTheorem(points: List[(Int, Int)]): Int =
    triangleFormulaArea(points) - (points.length / 2) + 1

  val lines = InputFileReader.getLinesFromFile("./src/main/scala/days/day10/input.txt").toList
  val pipeMap = parseInputMap(lines)
  val pipePath = findPipePath(pipeMap)
  solveForInteriorPointsWithPickersTheorem(pipePath)

def solvePipePath(): List[(Int, Int)] =
  val lines = InputFileReader.getLinesFromFile("./src/main/scala/days/day10/input.txt").toList
  val pipeMap = parseInputMap(lines)
  val pipePath = findPipePath(pipeMap)
  pipePath

def parseInputMap(lines: List[String]): PipeMap =
  def mapCharToMapTile(char: Char): MapTile =
    char match
      case 'S' => MapTile.STARTING_POSITION
      case '.' => MapTile.GROUND
      case '|' => MapTile.VERTICAL_PIPE
      case '-' => MapTile.HORIZONTAL_PIPE
      case 'L' => MapTile.NORTH_AND_EAST_PIPE
      case 'J' => MapTile.NORTH_AND_WEST_PIPE
      case 'F' => MapTile.SOUTH_AND_EAST_PIPE
      case '7' => MapTile.SOUTH_AND_WEST_PIPE
      case _ => throw RuntimeException(s"Unexpected MapTile input encountered: $char.")
  val mapArray = lines.map(line => line.toCharArray.map(mapCharToMapTile)).toArray
  PipeMap(map = mapArray)

def findPipePath(pipeMap: PipeMap): List[(Int, Int)] =
  def getStartingPositionRowAndColumnIndex: (Int, Int) =
    val startingPositionRow = pipeMap.map.find(_.contains(MapTile.STARTING_POSITION))
    startingPositionRow match
      case Some(row) =>
        val m = pipeMap.map.indexOf(row)
        val n = row.indexOf(STARTING_POSITION)
        (m, n)
      case None => throw RuntimeException("No Starting Position found in pipe map.")

  @tailrec
  def searchPipePath(currentPositionIndex: (Int, Int), acc: List[(Int, Int)]): List[(Int, Int)] =
    def findNextIndex(): (Int, Int) =
      def upConnections = Set(MapTile.VERTICAL_PIPE, MapTile.SOUTH_AND_WEST_PIPE, MapTile.SOUTH_AND_EAST_PIPE, MapTile.STARTING_POSITION)
      def rightConnections = Set(MapTile.HORIZONTAL_PIPE, MapTile.NORTH_AND_WEST_PIPE, MapTile.SOUTH_AND_WEST_PIPE, MapTile.STARTING_POSITION)
      def downConnections = Set(MapTile.VERTICAL_PIPE, MapTile.NORTH_AND_WEST_PIPE, MapTile.NORTH_AND_EAST_PIPE, MapTile.STARTING_POSITION)
      def leftConnections = Set(MapTile.HORIZONTAL_PIPE, MapTile.NORTH_AND_EAST_PIPE, MapTile.SOUTH_AND_EAST_PIPE, MapTile.STARTING_POSITION)
      def continuesUp(): Boolean =
        if currentPositionIndex._1 != 0 then
          val upTileIndex = (currentPositionIndex._1 - 1, currentPositionIndex._2)
          doConnectorsAlign(upTileIndex, downConnections, upConnections)
        else
          false
      def continuesRight(): Boolean =
        if currentPositionIndex._2 != pipeMap.mapWidth - 1 then
          val rightTileIndex = (currentPositionIndex._1, currentPositionIndex._2 + 1)
          doConnectorsAlign(rightTileIndex, leftConnections, rightConnections)
        else
          false
      def continuesDown(): Boolean =
        if currentPositionIndex._1 != pipeMap.mapHeight - 1 then
          val downTileIndex = (currentPositionIndex._1 + 1, currentPositionIndex._2)
          doConnectorsAlign(downTileIndex, upConnections, downConnections)
        else
          false
      def continuesLeft(): Boolean =
        if currentPositionIndex._2 != 0 then
          val leftTileIndex = (currentPositionIndex._1, currentPositionIndex._2 - 1)
          doConnectorsAlign(leftTileIndex, rightConnections, leftConnections)
        else
          false

      def doConnectorsAlign(nextTileIndex: (Int, Int), previousConnectors: Set[MapTile], nextConnectors: Set[MapTile]): Boolean =
        val currentTile = pipeMap.getByIndex(currentPositionIndex)
        val nextTile = pipeMap.getByIndex(nextTileIndex)
        acc.headOption match
          case Some(previousIndex) =>
            !acc.head.equals(nextTileIndex) && nextConnectors.contains(nextTile) && previousConnectors.contains(currentTile)
          case None =>
            nextConnectors.contains(nextTile) && previousConnectors.contains(currentTile)

      if continuesUp() then
        (currentPositionIndex._1 - 1, currentPositionIndex._2)
      else if continuesRight() then
        (currentPositionIndex._1, currentPositionIndex._2 + 1)
      else if continuesDown() then
        (currentPositionIndex._1 + 1, currentPositionIndex._2)
      else if continuesLeft() then
        (currentPositionIndex._1, currentPositionIndex._2 - 1)
      else
        throw RuntimeException(s"Next index not found in pipe path: $currentPositionIndex")

    findNextIndex() match
      case nextIndex if pipeMap.getByIndex(nextIndex) == MapTile.STARTING_POSITION =>
        currentPositionIndex :: acc
      case nextIndex =>
        searchPipePath(nextIndex, currentPositionIndex :: acc)

  val startingPositionIndex = getStartingPositionRowAndColumnIndex
  searchPipePath(startingPositionIndex, List.empty)
