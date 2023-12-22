package days.day9

import utils.InputFileReader

import scala.annotation.tailrec

@main
def main(args: String*): Unit =
  val lines = InputFileReader.getLinesFromFile("./src/main/scala/days/day9/input.txt")
  val sensorReadings = lines.map(parsePastSensorReadings)
  val futureSensorReadings = sensorReadings.map(calculateFutureSensorReading)
  val result = futureSensorReadings.sum
  println(result)

def parsePastSensorReadings(line: String): List[Int] =
  def digitRegex = """(-?\d+)""".r
  val sensorReadings = digitRegex.findAllMatchIn(line).map(_.matched.toInt).toList
  sensorReadings

def calculateFutureSensorReading(pastSensorReadings: List[Int]): Int =
  @tailrec
  def calculateAllDerivatives(xs: List[Int], acc: List[List[Int]] = List.empty[List[Int]]): List[List[Int]] =
    if xs.forall(_ == 0) then
      xs :: acc
    else
      val derivatives = calculateDerivatives(xs)
      calculateAllDerivatives(derivatives, xs :: acc)
  def calculateDerivatives(xs: List[Int]): List[Int] =
    xs match
      case x :: Nil => List(0)
      case x =>
        val rightPairs = xs.dropRight(1)
        val leftPairs = xs.drop(1)
        rightPairs.zip(leftPairs).foldRight(List.empty[Int])((pair, acc) => (pair._1 - pair._2) :: acc)

  val derivatives = calculateAllDerivatives(pastSensorReadings.reverse)
  derivatives.foldLeft(0)((acc, xs) => acc + xs.head)
