package day1

import java.io.{BufferedReader, FileReader}
import scala.util.Using
import scala.util.matching.Regex

@main
def main(args: String*): Unit =
  partOneSolution()

def partOneSolution(): Unit =
  val lines = readInputFile()
  val parsedResult = lines.map(parseElfLine)
  println(parsedResult.sum)

def readInputFile(): Seq[String] =
  Using.resource(new BufferedReader(new FileReader("./src/main/scala/day1/input.txt"))) { reader =>
    Iterator.continually(reader.readLine()).takeWhile(_ != null).toSeq
  }

def parseElfLine(string: String): Int =
  combineFirstAndSecondDigit.tupled(getFirstAndLastDigitInString(string))

def getFirstAndLastDigitInString(string: String): (Int, Int) =
  def regexPattern =
    "(?=(\\d|one|two|three|four|five|six|seven|eight|nine))".r
  def transformRegexMatchToInt(regexGroup: String): Int =
    regexGroup match
      case "1" | "one" => 1
      case "2" | "two" => 2
      case "3" | "three" => 3
      case "4" | "four" => 4
      case "5" | "five" => 5
      case "6" | "six" => 6
      case "7" | "seven" => 7
      case "8" | "eight" => 8
      case "9" | "nine" => 9
      case _ => throw RuntimeException("Unexpected regex group")

  val digitPattern: Regex = regexPattern
  val digitMatches = digitPattern.findAllMatchIn(string).toList
  if digitMatches.length == 1 then
    val firstAndLastDigit = transformRegexMatchToInt(digitMatches.head.group(1))
    (firstAndLastDigit, firstAndLastDigit)
  else
    val firstDigit = transformRegexMatchToInt(digitMatches.head.group(1))
    val lastDigit = transformRegexMatchToInt(digitMatches.last.group(1))
    (firstDigit, lastDigit)

def combineFirstAndSecondDigit(a: Int, b: Int) = (a * 10) + b
