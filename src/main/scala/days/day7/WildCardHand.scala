package days.day7

case class WildCardHand(cardsInput: List[Int], bidInput: Int) extends CardHand(cardsInput, bidInput):
  override def compare(that: CardHand): Int =
    def generateAllPossibleWildCardPermutations(cardHand: CardHand): List[List[Int]] =
      def replaceWildCards(cards: List[Int]): List[List[Int]] =
        def notWildCards = (2 until 11).toList ::: (12 to 14).toList
        cards.indexOf(1) match
          case wildCardIndex if wildCardIndex >= 0 =>
            val (beginning, wildCard :: end) = cards.splitAt(wildCardIndex): @unchecked
            val possibilities = notWildCards.flatMap(c => replaceWildCards(beginning ::: (c :: end)))
            possibilities
          case _ => List(cards)

      replaceWildCards(cardHand.cards)

    def getBestPossibleRank(cardHand: CardHand): Int =
      val allPossibilities = generateAllPossibleWildCardPermutations(cardHand)
      val bestPossibleRank = allPossibilities.map(cardHandTypeRank).max
      bestPossibleRank

    val thisBestPossibleRank = getBestPossibleRank(this)
    val thatBestPossibleRank = getBestPossibleRank(that)

    thisBestPossibleRank - thatBestPossibleRank match
      case 0 => compareHighCards(this.cards, that.cards)
      case bestPossibleCardHandTypesComparison => bestPossibleCardHandTypesComparison
