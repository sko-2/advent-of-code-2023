package days.day4

import utils.InputFileReader

import scala.annotation.tailrec
import scala.collection.immutable.HashMap
import scala.collection.immutable

@main
def main(args: String*): Unit =
  println(partOneSolution())
  println(partTwoSolution())

def partOneSolution(): Int =
  def updateScore(currentGameNumber: Int, currentScore: Int, winningNumbers: List[Int]): Int =
    if winningNumbers.contains(currentGameNumber) then
      if currentScore > 0 then currentScore * 2 else 1
    else
      currentScore

  val lines = InputFileReader.getLinesFromFile("./src/main/scala/days/day4/input.txt").toList
  val gameScoresTotal = lines.map(getGameScore(_, updateScore)).sum
  gameScoresTotal


def partTwoSolution(): Int =
  def updateScore(currentGameNumber: Int, currentScore: Int, winningNumbers: List[Int]): Int =
    if winningNumbers.contains(currentGameNumber) then
      currentScore + 1
    else
      currentScore

  val lines = InputFileReader.getLinesFromFile("./src/main/scala/days/day4/input.txt").toList
  val calculateGameScore = getGameScore(_, updateScore)

  @tailrec
  def playGame(currentIndex: Int, cardSetAcc: HashMap[Int, Int]): HashMap[Int, Int] =
    if currentIndex == lines.length then
      cardSetAcc
    else
      val gameScore = calculateGameScore(lines(currentIndex))
      val updatedCardSet = List.range(currentIndex + 1,  currentIndex + 1 + gameScore)
        .foldLeft(cardSetAcc)((acc, i) => acc.updated(i, acc(i) + acc(currentIndex)))
      playGame(currentIndex + 1, updatedCardSet)

  val initialCardSet = lines.indices.foldRight(HashMap[Int, Int]())((i, acc) => acc.updated(i, 1))
  val finalCardSet = playGame(0, initialCardSet)
  finalCardSet.foldLeft(0)(_+_._2)

def getGameScore(line: String, updateScoreFn: (Int, Int, List[Int]) => Int): Int =
  val (winningNumbers, gameNumbers) = parseWinningNumbersAndGameNumbers(line)
  gameNumbers.fold(0)((acc, x) => updateScoreFn(x, acc, winningNumbers))

def parseWinningNumbersAndGameNumbers(line: String): (List[Int], List[Int]) =
  def getNumberMatches(line: String): List[Int] =
    "(\\d+)".r.findAllMatchIn(line)
      .map(n => n.matched.toInt)
      .toList

  val gameInfoString = line.split(':')(1).split('|')
  val winningNumbersString = gameInfoString(0)
  val gameNumbersString = gameInfoString(1)
  val winningNumbers = getNumberMatches(winningNumbersString)
  val gameNumbers = getNumberMatches(gameNumbersString)
  (winningNumbers, gameNumbers)
