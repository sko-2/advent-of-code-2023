package days.day7

import scala.annotation.tailrec

case class CardHand(cards: List[Int], bid: Int) extends Ordered[CardHand]:
  require(cards.length == 5, "CardHand requires exactly  5 cards.")

  override def compare(that: CardHand): Int =
    def compareCardHandTypes: Int =
      def cardHandTypeRank(cards: List[Int]): Int =
        extension (cards: Map[Int, Int])
          private def containsCardNTimes(n: Int): Boolean =
            cards.exists((_, numberOfCards) => numberOfCards == n)
        def isFiveOfAKind(cards: Map[Int, Int]): Boolean =
          cards.containsCardNTimes(5)
        def isFourOfAKind(cards: Map[Int, Int]): Boolean =
          cards.containsCardNTimes(4)
        def isFullHouse(cards: Map[Int, Int]): Boolean =
          cards.containsCardNTimes(3) && cards.containsCardNTimes(2)
        def isThreeOfAKind(cards: Map[Int, Int]): Boolean =
          cards.containsCardNTimes(3)
        def isTwoPairs(cards: Map[Int, Int]): Boolean =
          cards.count((_, numberOfCards) => numberOfCards == 2) == 2
        def isPair(cards: Map[Int, Int]): Boolean =
          cards.containsCardNTimes(2)
        cards.groupBy(identity).map(c => c._1 -> c._2.size) match
          case x: Map[Int, Int] if isFiveOfAKind(x) => 6
          case x: Map[Int, Int] if isFourOfAKind(x) => 5
          case x: Map[Int, Int] if isFullHouse(x) => 4
          case x: Map[Int, Int] if isThreeOfAKind(x) => 3
          case x: Map[Int, Int] if isTwoPairs(x) => 2
          case x: Map[Int, Int] if isPair(x) => 1
          case _ => 0

      cardHandTypeRank(this.cards) - cardHandTypeRank(that.cards)

    @tailrec
    def compareHighCards(thisCards: List[Int], thatCards: List[Int]): Int =
      (thisCards.headOption, thatCards.headOption) match
        case (Some(thisCard), Some(thatCard)) =>
          thisCard - thatCard match
            case 0 => compareHighCards(thisCards.tail, thatCards.tail)
            case cardComparison => cardComparison
        case (None, None) => 0
        case _ => throw RuntimeException(s"Invalid compareHighCards function call: thisCards: $thisCards | thatCards: $thatCards")

    compareCardHandTypes match
      case 0 => compareHighCards(this.cards, that.cards)
      case cardHandTypeComparison => cardHandTypeComparison
