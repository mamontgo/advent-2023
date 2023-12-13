package mamontgo.advent.day7

import org.scalatest.funsuite.AnyFunSuite

import scala.io.Source

class CamelPokerTest extends AnyFunSuite {


  test("test enum") {
    println("hello")
    println(PlayingCard.TEN)
    print(PlayingCard.values.toList.foreach(c => println(c)))
  }

  test("read example hands with sort by score") {
    val r: Seq[PrizeDeal] = CamelPoker.processLines(Source.fromFile("./src/main/resources/day7/example.txt").getLines().toSeq)
    println(r)
    r.map(p => p.hand).foreach(d => println(d.handScore))
    println("Sorted now?")
    PrizeDeal.sort(r).foreach(d => println(d.hand.handScore))
  }

  test("read example 2 hands") {
    val r: Seq[PrizeDeal] = CamelPoker.processLines(Source.fromFile("./src/main/resources/day7/example2.txt").getLines().toSeq)
    println(r)
    r.map(p => p.hand).foreach(d => println(d.handScore))
  }


  test("secondRank example 3 test") {
    val r: Seq[PrizeDeal] = CamelPoker.processLines(Source.fromFile("./src/main/resources/day7/example3.txt").getLines().toSeq)

    val res = RankedPrizeDeal.rank(r)

    println(res)
  }

  test("calc example ranking bug") {
    val r: Seq[PrizeDeal] = CamelPoker.processLines(Source.fromFile("./src/main/resources/day7/second.txt").getLines().toSeq)
    r.foreach(f => println(s" rank ${f.hand.cards} card score ${f.hand.cardScore}"))
  }


  test("calc example total prize") {
    val r: Seq[PrizeDeal] = CamelPoker.processLines(Source.fromFile("./src/main/resources/day7/example.txt").getLines().toSeq)
    val res = RankedPrizeDeal.calcPrize(r)

    println(res)
    assert(res == 6440)
  }




  test("calc example total prize solution part 1") {
    val r: Seq[PrizeDeal] = CamelPoker.processLines(Source.fromFile("./src/main/resources/day7/test.txt").getLines().toSeq)
    val res = RankedPrizeDeal.calcPrize(r)

    println(res)
    assert(res == 251029473)
  }


  test("calc example total prize with joker") {
    val r: Seq[PrizeDeal] = CamelPoker.processLines(Source.fromFile("./src/main/resources/day7/example.txt").getLines().toSeq)
    val res = RankedPrizeDeal.jokerCalcPrize(r)

    println(res)
    assert(res == 5905)
  }



  test("calc example joker ranking bug") {
    val r: Seq[PrizeDeal] = CamelPoker.processLines(Source.fromFile("./src/main/resources/day7/test.txt").getLines().toSeq)
    r.foreach(f => println(s" rank ${f.hand.cards} card score ${f.hand.jokerCardScore}"))
  }

  test("calc solution total prize with joker") {
    val r: Seq[PrizeDeal] = CamelPoker.processLines(Source.fromFile("./src/main/resources/day7/test.txt").getLines().toSeq)

    val temp = PrizeDeal.jokerSort(r);

    temp.foreach(
      l => println(s"Cards: ${l.hand.cards} - handScore: ${l.hand.jokerHandScore} - cardScore: ${l.hand.jokerCardScore} - totalScore: ${l.hand.jokerTotalScore}")
    )

    val res = RankedPrizeDeal.jokerCalcPrize(r)

    println(res)
    assert(res == 251363551) // to high
  }
}
