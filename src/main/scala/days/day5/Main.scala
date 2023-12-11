package days.day5

import utils.InputFileReader

import scala.annotation.tailrec
import scala.collection.immutable.HashMap

@main
def main(args: String*): Unit =
  val result = partTwoSolution()
  println(result)

def partOneSolution(): Long =
  val lines = InputFileReader.getLinesFromFile("./src/main/scala/days/day5/input.txt").toList
  lines match
    case seedLine :: blankLine :: xs =>
      val seeds = getSeeds(seedLine)
      val mappings = getMappings(xs)
      val locations = transformSeedsToLocations(seeds, mappings)
      locations.min
    case _ =>
      throw RuntimeException("Unexpected input format")

def partTwoSolution(): Long =
  @tailrec
  def searchForLowestLocation(locationIndex: Long, seedRanges: List[(Long, Long)], locationToSeedFn: Long => Long): Long =
    val seedIndex = locationToSeedFn(locationIndex)
    seedRanges.find(p => seedIndex >= p._1 && seedIndex < (p._1 + p._2)) match
      case Some(x) => locationIndex
      case None => searchForLowestLocation(locationIndex + 1, seedRanges, locationToSeedFn)
  val lines = InputFileReader.getLinesFromFile("./src/main/scala/days/day5/input.txt").toList
  lines match
    case seedLine :: blankLine :: xs =>
      val seedRanges = getSeedRanges(seedLine)
      val mappings = getMappings(xs)
      val locationToSeedFn = transformLocationToSeed(_, mappings)
      searchForLowestLocation(0L, seedRanges, locationToSeedFn)
    case _ =>
      throw RuntimeException("Unexpected input format")

def transformLocationToSeed(location: Long, mappings: HashMap[(String, String), List[MappingDetail]]): Long =
  def getSourceOfDestination(destination: String): String =
    val nextMapping = mappings.find((x, _) => x._2 == destination)
    nextMapping match
      case Some(m) => m._1._1
      case None => throw RuntimeException("Expected mapping not found")

  @tailrec
  def transformDestinationToSeed(destination: String, input: Long): Long =
    if destination == "seed" then
      input
    else
      val source = getSourceOfDestination(destination)
      val transform = transformDestinationToSource(source, destination, input)
      transformDestinationToSeed(source, transform)

  def transformDestinationToSource(source: String, destination: String, input: Long): Long =
    val relevantMapping = mappings((source, destination)).findLast(m => m.isInverseApplicable(input))
    relevantMapping match
      case Some(m) => m.applyInverse(input)
      case None => input

  val destination = "location"
  transformDestinationToSeed(destination, location)

def transformSeedsToLocations(
  seeds: List[Long],
  mappings: HashMap[(String, String), List[MappingDetail]]
): List[Long] =
  def getDestinationOfSource(source: String): String =
    val nextMapping = mappings.find((x, _) => x._1 == source)
    nextMapping match
      case Some(m) => m._1._2
      case None => throw RuntimeException("Expected mapping not found")

  @tailrec
  def transformSourceToLocation(source: String, input: Long): Long =
    if source == "location" then
      input
    else
      val destination = getDestinationOfSource(source)
      val transform = transformSourceToDestination(source, destination, input)
      transformSourceToLocation(destination, transform)

  def transformSourceToDestination(source: String, destination: String, input: Long): Long =
    val relevantMapping = mappings((source, destination)).findLast(m => m.isApplicable(input))
    relevantMapping match
      case Some(m) => m.apply(input)
      case None => input

  val source = "seed"
  seeds.map(transformSourceToLocation(source, _))

def getSeeds(line: String): List[Long] =
  require(line.startsWith("seeds: "))
  getDigits(line)

def getSeedRanges(line: String): List[(Long, Long)] =
  @tailrec
  def parseInputs(inputs: List[Long], acc: List[(Long, Long)]): List[(Long, Long)] =
    inputs match
      case start :: length :: xs =>
        val updatedAcc = (start, length) :: acc
        parseInputs(xs, updatedAcc)
      case _ => acc

  val seedInputs = getSeeds(line)
  parseInputs(seedInputs, List.empty)

def seedRangeToSeedList(start: Long, length: Long): List[Long] =
  (start to (start + length)).toList

def getDigits(line: String): List[Long] =
  def digitsRegex = """(\d+)""".r

  val digitMatches = digitsRegex.findAllMatchIn(line).toList
  digitMatches.map(_.matched.toLong)

def getSourceAndDestination(line: String): (String, String) =
  def mapperRegex = """^([a-z]+)-to-([a-z]+) map:$""".r

  val parsedLine = mapperRegex.findFirstMatchIn(line)
  parsedLine match
    case Some(x) => (x.group(1), x.group(2))
    case None => throw RuntimeException(s"Unexpected Source and Destination Input:\n$line")

def getMappings(lines: List[String]): HashMap[(String, String), List[MappingDetail]] =
  def getMappingDetails(line: String): MappingDetail =
    val digits = getDigits(line)
    MappingDetail(
      sourceStartingIndex = digits(1),
      destinationStartingIndex = digits.head,
      numberOfMatchingIndices = digits(2)
    )
  def parseMapper(detailLines: List[String]): List[MappingDetail] =
    detailLines.map(getMappingDetails)
  @tailrec
  def getMappings(
    lines: List[String],
    mappingsAcc: HashMap[(String, String), List[MappingDetail]]
  ): HashMap[(String, String), List[MappingDetail]] =
    lines match
      case Nil => mappingsAcc
      case x :: xs =>
        val sourceAndDestination = getSourceAndDestination(x)
        val mappingInput = xs.takeWhile(_.nonEmpty)
        val mappings = parseMapper(mappingInput)
        val updatedMappingsAcc = mappingsAcc.updated(sourceAndDestination, mappings)
        getMappings(xs.drop(mappingInput.length + 1), updatedMappingsAcc)
  getMappings(lines, HashMap.empty)
