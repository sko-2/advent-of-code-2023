package days.day7

import scala.annotation.tailrec

case class StandardCardHand(cardsInput: List[Int], bidInput: Int) extends CardHand(cardsInput, bidInput):
  override def compare(that: CardHand): Int =
    compareCardHandTypes(that) match
      case 0 => compareHighCards(this.cards, that.cards)
      case cardHandTypeComparison => cardHandTypeComparison
