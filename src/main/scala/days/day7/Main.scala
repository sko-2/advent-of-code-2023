package days.day7

import utils.InputFileReader

import scala.util.Sorting

@main
def main(args: String*): Unit =
  val result = partOneSolution()
  println(result)

def partOneSolution(): Int =
  def parseCardHands(): List[CardHand] =
    val lines = InputFileReader.getLinesFromFile("./src/main/scala/days/day7/input.txt").toList
    val cardHands = lines.map(parseCardHand)
    cardHands

  def sortCardHands(cardHands: List[CardHand]): List[CardHand] =
    val cardHandsArray = cardHands.toArray
    Sorting.quickSort(cardHandsArray)
    val sortedCardHands = cardHandsArray.toList
    sortedCardHands

  def calculateWinnings(cardHands: List[CardHand]): Int =
    val sortedCardHands = sortCardHands(cardHands)
    val scores = sortedCardHands
      .zipWithIndex
      .map((cardHand, index) =>
        val rank = index + 1
        cardHand.bid * rank
      )
    val winnings = scores.sum
    winnings

  val cardHands = parseCardHands()
  calculateWinnings(cardHands)

def parseCardHand(input: String): CardHand =
  def cardHandRegex = """(\w{5}) (\d+)""".r

  def parseCardsInHand(cardsGroup: String): List[Int] =
    def parseCardRawValue(card: Char): Int =
      card match
        case c if c.isDigit && 1 < c.toString.toInt && c.toString.toInt < 10 => c.toString.toInt
        case 'T' => 10
        case 'J' => 11
        case 'Q' => 12
        case 'K' => 13
        case 'A' => 14
        case _ => throw RuntimeException(s"Invalid card character in input: $cardsGroup | Problem character: $card")
    val cardChars = cardsGroup.toUpperCase.toList
    cardChars.map(parseCardRawValue)

  val cardHandMatch = cardHandRegex.findAllMatchIn(input).nextOption()
  cardHandMatch match
    case Some(m) => CardHand(parseCardsInHand(m.group(1)), m.group(2).toInt)
    case None => throw RuntimeException(s"Failed parsing of card hand line: $input")
    