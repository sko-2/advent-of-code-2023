package days.day8

import days.day8.Direction.{LEFT, RIGHT}
import utils.InputFileReader

import scala.annotation.tailrec
import scala.util.matching.Regex

@main
def main(args: String*): Unit =
  val result = partOneSolution()
  println(result)

def partOneSolution(): Int =
  val lines = InputFileReader.getLinesFromFile("./src/main/scala/days/day8/input.txt")
  val directions = parseDirections(lines.head)
  val nodeMap = lines.drop(2).map(parseNodeDefinition).toMap
  countStepsToEnd(directions, nodeMap)

def parseDirections(line: String): List[Direction] =
  def charToDirection(char: Char): Direction =
    char match
      case 'L' => Direction.LEFT
      case 'R' => Direction.RIGHT
      case _ => throw RuntimeException(s"Unexpected direction character encountered: $char.")

  line.toList.map(charToDirection)

def parseNodeDefinition(line: String): (String, Node) =
  def nodeDefinitionRegex: Regex = """(\w{3}) = \((\w{3}), (\w{3})\)""".r

  val nodeMatch = nodeDefinitionRegex.findFirstMatchIn(line)
  nodeMatch match
    case Some(m) =>
      val nodeName = m.group(1)
      val leftChildNode = m.group(2)
      val rightChildNode = m.group(3)
      (nodeName, Node(leftChildNode, rightChildNode))
    case _ => throw RuntimeException(s"Did not find expected nodeDefinitionRegex match in input: $line")

def countStepsToEnd(directions: List[Direction], nodeMap: Map[String, Node]): Int =
  @tailrec
  def findStepsToDestination(startNode: String, acc: Int = 1): Int =
    def traverse(direction: Direction, node: Node): String =
      direction match
        case LEFT => node.left
        case RIGHT => node.right

    def followDirections(startNode: String): String =
      directions.foldLeft(startNode)((currentNode, direction) => traverse(direction, nodeMap(currentNode)))

    val destinationAfterFollowingDirections = followDirections(startNode)
    if destinationAfterFollowingDirections == "ZZZ" then
      acc
    else
      findStepsToDestination(destinationAfterFollowingDirections, acc + 1)

  val numberOfTimesNeededToFollowDirections = findStepsToDestination("AAA")
  numberOfTimesNeededToFollowDirections * directions.length
