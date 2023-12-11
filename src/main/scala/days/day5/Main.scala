package days.day5

import utils.InputFileReader

import scala.annotation.tailrec
import scala.collection.immutable.HashMap

@main
def main(args: String*): Unit =
  val lines = InputFileReader.getLinesFromFile("./src/main/scala/days/day5/input.txt").toList
  lines match
    case seedLine :: blankLine :: xs =>
      val seeds = getSeeds(seedLine)
      val mappings = getMappings(xs)
      val locations = transformSeedsToLocations(seeds, mappings)
      println(locations.min)
    case _ =>
      throw RuntimeException("Unexpected input format")

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
