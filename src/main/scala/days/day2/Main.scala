package days.day2

import utils.InputFileReader

@main
def main(args: String*): Unit =
  println(partTwoSolution)

def partOneSolution: Int =
  val lines = InputFileReader.getLinesFromFile("./src/main/scala/days/day2/input.txt")
  val gameResults = lines.flatMap(isGameIdValid(_, 14, 13, 12))
  gameResults.sum

def partTwoSolution: Int =
  def getMaxBlueGreenAndRed(line: String): (Int, Int, Int) =
    (getBlueMax(line), getGreenMax(line), getRedMax(line))
  def setOfCubesPower(x: Int, y: Int, z: Int): Int = x * y * z
  val lines = InputFileReader.getLinesFromFile("./src/main/scala/days/day2/input.txt")
  val gameMinimums = lines.map(getMaxBlueGreenAndRed)
  gameMinimums.map(setOfCubesPower).sum


def isGameIdValid(line: String, maxBlue: Int, maxGreen: Int, maxRed: Int): Option[Int] =
  def getGameId(line: String): Int =
    val gameIdRegex = "(Game (\\d+))".r
    gameIdRegex.findFirstMatchIn(line).head.group(2).toInt

  val gameId = getGameId(line)
  if getBlueMax(line) <= maxBlue && getGreenMax(line) <= maxGreen && getRedMax(line) <= maxRed then
    Some(gameId)
  else
    None

def getMaxColorDraws(line: String, color: String): Int =
  val drawRegex = s"((\\d+) $color)".r
  val colorDraws = drawRegex.findAllMatchIn(line).toList
  colorDraws.foldRight(0)((x, acc) => Math.max(acc, x.group(2).toInt))
def getBlueMax(line: String): Int =
  getMaxColorDraws(line, Color.BLUE)
def getGreenMax(line: String): Int =
  getMaxColorDraws(line, Color.GREEN)
def getRedMax(line: String): Int =
  getMaxColorDraws(line, Color.RED)
