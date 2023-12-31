package days.day3

import utils.InputFileReader

import scala.annotation.tailrec
import scala.util.matching.Regex
import scala.util.matching.Regex.Match

@main
def main(args: String*): Unit =
  partTwoSolution()

def partOneSolution(): Unit =
  val paddedInput = loadAndPadInput()
  val result = sumPartNumbers(paddedInput)
  println(result)

def partTwoSolution(): Unit =
  val paddedInput = loadAndPadInput()
  val result = sumGearRatios(paddedInput)
  println(result)

def loadAndPadInput(): Array[String] =
  val lines = InputFileReader.getLinesFromFile("./src/main/scala/days/day3/input.txt").toArray
  val paddedLine = ".".repeat(lines.head.length)
  Array(paddedLine) ++ lines ++ Array(paddedLine)

def sumPartNumbers(lines: Array[String]): Int =
  @tailrec
  def scanAndSumPartNumbers(currentIndex: Int, acc: Int): Int =
    def linePartNumberSum(): Int =
      val numberMatches = getNumberMatches(lines(currentIndex))
      numberMatches
        .filter(m => {
          isPartNumber(m)
        })
        .map(_.matched.toInt)
        .sum

    def isPartNumber(partNumberMatch: Match): Boolean =
      val previousLineTouchingSymbolMatches = getSymbolMatches(lines(currentIndex - 1))
        .filter(m => m.start >= partNumberMatch.start - 1 && m.end <= partNumberMatch.end + 1)
      val currentLineTouchingSymbolMatches = getSymbolMatches(lines(currentIndex))
        .filter(m => m.start == partNumberMatch.end || m.end == partNumberMatch.start)
      val nextLineTouchingSymbolMatches = getSymbolMatches(lines(currentIndex + 1))
        .filter(m => m.start >= partNumberMatch.start - 1 && m.end <= partNumberMatch.end + 1)
      previousLineTouchingSymbolMatches.nonEmpty
        || currentLineTouchingSymbolMatches.nonEmpty
        || nextLineTouchingSymbolMatches.nonEmpty

    if currentIndex == (lines.length - 1) then
      acc
    else
      val partNumberSum = linePartNumberSum()
      scanAndSumPartNumbers(currentIndex + 1, acc + partNumberSum)
  scanAndSumPartNumbers(1, 0)

def sumGearRatios(lines: Array[String]): Int =
  @tailrec
  def scanAndSumGearRatios(currentIndex: Int, acc: Int): Int =
    def lineGearRatioSum(): Int =
      val gearMatches = getGearMatches(lines(currentIndex))
      gearMatches
        .map(g => calculateGearRatio(g))
        .sum

    def calculateGearRatio(gearMatch: Match): Int =
      val previousLinePartNumbersTouchingGearMatches = getNumberMatches(lines(currentIndex - 1))
        .filter(m => (m.start until m.end).intersect(gearMatch.start - 1 to gearMatch.end).nonEmpty)
      val currentLineTouchingSymbolMatches = getNumberMatches(lines(currentIndex))
        .filter(m => m.start == gearMatch.end || m.end == gearMatch.start)
      val nextLinePartNumbersTouchingGearMatches = getNumberMatches(lines(currentIndex + 1))
        .filter(m => (m.start until m.end).intersect(gearMatch.start - 1 to gearMatch.end).nonEmpty)

      val touchingPartNumbers = previousLinePartNumbersTouchingGearMatches
          ::: currentLineTouchingSymbolMatches
          ::: nextLinePartNumbersTouchingGearMatches

      if touchingPartNumbers.length == 2 then
        touchingPartNumbers
          .map(p => p.matched.toInt)
          .product
      else
        0

    if currentIndex == (lines.length - 1) then
      acc
    else
      val gearRatioSum = lineGearRatioSum()
      scanAndSumGearRatios(currentIndex + 1, acc + gearRatioSum)
  scanAndSumGearRatios(1, 0)

def getNumberMatches(line: String): List[Match] =
  getRegexMatches("(\\d+)".r, line)

def getSymbolMatches(line: String): List[Match] =
  getRegexMatches("([^\\d.])".r, line)

def getGearMatches(line: String): List[Match] =
  getRegexMatches("(\\*)".r, line)

def getRegexMatches(regex: Regex, line: String): List[Match] =
  regex.findAllMatchIn(line).toList
