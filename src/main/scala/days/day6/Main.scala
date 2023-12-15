package days.day6

import utils.InputFileReader

@main
def main(args: String*): Unit =
  val result = partTwoSolution()
  println(result)

def partOneSolution(): Long =
  def parseTimesAndDistances(timeLine: String, distanceLine: String): List[(Long, Long)] =
    val times = getDigits(timeLine)
    val distances = getDigits(distanceLine)
    times.zip(distances)

  solve(parseTimesAndDistances)

def partTwoSolution(): Long =
  def parseTimesAndDistances(timeLine: String, distanceLine: String): List[(Long, Long)] =
    def combineNumbersOnLine(line: String): Long =
      getDigits(line).foldLeft("")((acc, x) => acc + x).toLong
    val time = combineNumbersOnLine(timeLine)
    val distance = combineNumbersOnLine(distanceLine)
    List((time, distance))

  solve(parseTimesAndDistances)

def getNumberOfSolutions(raceLength: Long, distanceToBeat: Long): Long =
  val candidateTest = isDistanceFurtherThanRecord(_, raceLength, distanceToBeat)
  (1L until raceLength)
    .count(candidateTest(_))

def isDistanceFurtherThanRecord(buttonPressedTime: Long, raceLength: Long, distanceToBeat: Long): Boolean =
  buttonPressedTime * (raceLength - buttonPressedTime) > distanceToBeat

def solve(timesAndDistancesParser: (String, String) => List[(Long, Long)]): Long =
  val lines = InputFileReader.getLinesFromFile("./src/main/scala/days/day6/input.txt")
  val timesAndDistances = timesAndDistancesParser(lines.head, lines.last)
  timesAndDistances
    .map(getNumberOfSolutions.tupled(_))
    .product

def getDigits(line: String): List[Long] =
  def digitsRegex = """(\d+)""".r
  val digitMatches = digitsRegex.findAllMatchIn(line).toList
  digitMatches.map(_.matched.toLong)
