package mamontgo.advent.day7

import mamontgo.advent.day7.Hand.HandEvaluate
import mamontgo.advent.util.Ftils


case class Deal(cards: Seq[PlayingCard.Value]) {

  lazy val totalScore: Long = (handScore * 100000000000000L) + cardScore
  lazy val handScore: Int = Hand.score(this)

  lazy val cardScore: Long = cards.reverse.foldLeft((0L, 1l)) { (i, v) => {
    (i._1 + (Card.cardRanks.getOrElse(v, 0) * i._2), i._2 * 1000)
  } }._1

  val groups: Map[PlayingCard.Value, Int] = cards.foldLeft(Map.empty[PlayingCard.Value, Int]) { (i, v) => i.+((v, i.getOrElse(v, 0) + 1)) }


  lazy val jokers: Int = groups.getOrElse(PlayingCard.J, 0)
  lazy val jokerGroup: Map[PlayingCard.Value, Int] = groups.removed(PlayingCard.J)
  lazy val jokerHandScore: Int = Hand.scoreJoker(this)

  lazy val jokerCardScore: Long = cards.reverse.foldLeft((0L, 1l)) { (i, v) => {
    (i._1 + (Card.jokerCardRanks.getOrElse(v, 0) * i._2), i._2 * 1000)
  } }._1

  lazy val jokerTotalScore: Long = (jokerHandScore * 100000000000000L) + jokerCardScore
}

case class RankedPrizeDeal(prizeDeal: PrizeDeal, rank: Long) {}

object RankedPrizeDeal {

  def calcPrize(hands: Seq[PrizeDeal]): Long = {
    rank(hands).foldLeft(0L) { (i, v) => i + (v.prizeDeal.prize * v.rank) }
  }

  def jokerCalcPrize(hands: Seq[PrizeDeal]): Long = {
    jokerRank(hands).foldLeft(0L) { (i, v) => i + (v.prizeDeal.prize * v.rank) }
  }


  def rank(hands: Seq[PrizeDeal]): Seq[RankedPrizeDeal] = {
    ranking(PrizeDeal.sort(hands))
  }

  def jokerRank(hands: Seq[PrizeDeal]): Seq[RankedPrizeDeal] = {
    ranking(PrizeDeal.jokerSort(hands))
  }

  private def ranking(sorted: Seq[PrizeDeal]): Seq[RankedPrizeDeal] = {
    sorted.foldLeft(Seq[RankedPrizeDeal]()) { (i, v) => if (i.isEmpty) Seq(RankedPrizeDeal(v, 1)) else i ++ Seq(RankedPrizeDeal(v, i.last.rank + 1)) }
  }
}

case class PrizeDeal(hand: Deal, prize: Long) {}

object PrizeDeal {
  def sort(prizeDeals: Seq[PrizeDeal]): Seq[PrizeDeal] = {
    prizeDeals.sortWith(_.hand.totalScore < _.hand.totalScore)
  }

  def jokerSort(prizeDeals: Seq[PrizeDeal]): Seq[PrizeDeal] = {
    prizeDeals.sortWith(_.hand.jokerTotalScore < _.hand.jokerTotalScore)
  }
}

object Card {
  lazy val cardRanks: Map[PlayingCard.Value, Int] = Map(PlayingCard.A -> 14, PlayingCard.K -> 13, PlayingCard.Q -> 12, PlayingCard.J -> 11, PlayingCard.TEN -> 10,
    PlayingCard.NINE -> 9, PlayingCard.EIGHT -> 8, PlayingCard.SEVEN -> 7, PlayingCard.SIX -> 6, PlayingCard.FIVE -> 5, PlayingCard.FOUR -> 4,
    PlayingCard.THREE -> 3, PlayingCard.TWO -> 2)
  lazy val jokerCardRanks: Map[PlayingCard.Value, Int] = cardRanks.removed(PlayingCard.J) + (PlayingCard.J -> 1)
}

object Hand {
  type HandEvaluate = Deal => Boolean

  private lazy val fiveOfKind: HandEvaluate = d => d.groups.values.exists(i => i == 5)
  private lazy val fourOfKind: HandEvaluate = d => d.groups.values.exists(i => i == 4)
  private lazy val fullHouse: HandEvaluate = d => d.groups.values.exists(i => i == 3) && d.groups.values.exists(i => i == 2)
  private lazy val threeOfKind: HandEvaluate = d => d.groups.values.exists(i => i == 3)
  private lazy val twoPair: HandEvaluate = d => d.groups.values.count(i => i == 2) == 2
  private lazy val pair: HandEvaluate = d => d.groups.values.count(i => i == 2) == 1
  private lazy val highCard: HandEvaluate = d => true

  private lazy val fiveOfKindJoker: HandEvaluate = d => d.jokerGroup.values.exists(i => i == 5) || d.jokerGroup.values.exists(i => d.jokers + i == 5) || d.jokers == 5
  private lazy val fourOfKindJoker: HandEvaluate = d => d.jokerGroup.values.exists(i => i == 4) || d.jokerGroup.values.exists(i => d.jokers + i == 4)
  private lazy val fullHouseJoker: HandEvaluate = d => d.jokerGroup.values.exists(i => i == 3) && d.jokerGroup.values.exists(i => i == 2) ||
        d.jokerGroup.values.count(i => i == 2) == 2  && d.jokers == 1
  private lazy val threeOfKindJoker: HandEvaluate = d => d.jokerGroup.values.exists(i => i == 3) || d.jokerGroup.values.exists(i => d.jokers + i == 3)
  private lazy val twoPairJoker: HandEvaluate = twoPair
  private lazy val pairJoker: HandEvaluate = d => d.jokerGroup.values.count(i => i == 2) == 1 || d.jokerGroup.values.count(i => i > 1) == 0 && d.jokers == 1

  lazy val hands: Seq[Hand] = Seq(
    Hand("Five Of Kind", 1000, fiveOfKind, fiveOfKindJoker),
    Hand("Four Of Kind", 900, fourOfKind, fourOfKindJoker),
    Hand("Full House", 800, fullHouse, fullHouseJoker),
    Hand("Three of kind", 700, threeOfKind, threeOfKindJoker),
    Hand("Two Pair", 600, twoPair, twoPairJoker),
    Hand("Pair", 500, pair, pairJoker),
    Hand("High Card", 400, highCard, highCard)
  )


  def score(d: Deal): Int = {
    Ftils.foldUntil[Hand, Int](hands, 0, i => i > 0) { (i, h) => if (h.eval(d)) h.score else i}
  }

  def scoreJoker(d: Deal): Int = {
    Ftils.foldUntil[Hand, Int](hands, 0, i => i > 0) {(i, h) => if (h.jokerEval(d)) h.score else i}
  }
}

object CamelPoker {

  def processLines(lines: Seq[String]): Seq[PrizeDeal] = {
    lines.filter(s => s.nonEmpty).map(l => l.split(' ')).map(a => PrizeDeal(Deal(a(0).map(c => PlayingCard.values.find(v => v.toString == c.toString).getOrElse(PlayingCard.TEN))), a(1).toLong))
  }

}

case class Hand(name: String, score: Int, eval: HandEvaluate, jokerEval: HandEvaluate)

object PlayingCard extends Enumeration {
  val A = Value("A")
  val K = Value("K")
  val Q = Value("Q")
  val J = Value("J")
  val TEN = Value("T")
  val NINE = Value("9")
  val EIGHT = Value("8")
  val SEVEN = Value("7")
  val SIX = Value("6")
  val FIVE = Value("5")
  val FOUR = Value("4")
  val THREE = Value("3")
  val TWO = Value("2")
}