package days.day2

import utils.InputFileReader

@main
def main(args: String*): Unit =
  val lines = InputFileReader.getLinesFromFile("./src/main/scala/days/day2/input.txt")
  val gameResults = lines.flatMap(isGameIdValid(_, 14, 13, 12))
  println(gameResults.sum)

def isGameIdValid(line: String, maxBlue: Int, maxGreen: Int, maxRed: Int): Option[Int] =
  def getGameId(line: String): Int =
    val gameIdRegex = "(Game (\\d+))".r
    gameIdRegex.findFirstMatchIn(line).head.group(2).toInt
  def getTotalColorDraws(line: String, color: String): Int =
    val drawRegex = s"((\\d+) $color)".r
    val colorDraws = drawRegex.findAllMatchIn(line).toList
    colorDraws.foldRight(0)((x, acc) => Math.max(acc, x.group(2).toInt))
  def getBlueMax(line: String): Int =
    getTotalColorDraws(line, Color.BLUE)
  def getGreenMax(line: String): Int =
    getTotalColorDraws(line, Color.GREEN)
  def getRedMax(line: String): Int =
    getTotalColorDraws(line, Color.RED)

  val gameId = getGameId(line)
  if getBlueMax(line) <= maxBlue && getGreenMax(line) <= maxGreen && getRedMax(line) <= maxRed then
    Some(gameId)
  else
    None
