package mamontgo.advent.day4

import org.scalatest.funsuite.AnyFunSuite

import scala.io.Source

class Day4Tests  extends AnyFunSuite {

  test("input test") {
    val c = Card.fromString("Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53")
    assert(Card.calcPoints(c) == 8)
  }

  test("input card scores") {
    val source = Source.fromFile("./src/main/resources/day4/example.txt")
    val cards = source.getLines().map(l => Card.fromString(l)).toList

    val res = cards.map(c => Card.calcPoints(c))

    println(res)
    assert(res.sum == 13)

  }


  test("count") {
    val c = Card.fromString("Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53")
    assert(Card.countMatches(c) == 4)
  }


  test("input card scores test") {

    val source = Source.fromFile("./src/main/resources/day4/test.txt")
    val cards = source.getLines().map(l => Card.fromString(l)).toList

    val res = cards.map(c => Card.calcPoints(c))

    println(res)
    println(res.sum)

    assert(res.sum == 25010)

  }

  test("trioubleshooting") {
    val x = "Card   1: 13  5 40 15 21 61 74 55 32 56 | 21 57 74 56  7 84 37 47 75 66 68  8 55 22 53 61 40 13 15 41 32 46 95 65  5"
    val res = Card.fromString(x)
  }

  test("recursive test") {
    val source = Source.fromFile("./src/main/resources/day4/example.txt")
    val cards = source.getLines().map(l => Card.fromString(l)).toList

    val res = Card.calcCards(cards)
    println(res)
  }


  test("countCard") {
    val source = Source.fromFile("./src/main/resources/day4/example.txt")
    val cards = source.getLines().map(l => Card.fromString(l)).toList

    val res = Card.countCard(cards.head, cards.tail)
    println(res)

    val res2 = Card.countAllCards(cards)
    println(res2)
  }

  test("countAllCards") {
    val source = Source.fromFile("./src/main/resources/day4/test.txt")
    val cards = source.getLines().map(l => Card.fromString(l)).toList

    val res2 = Card.countAllCards(cards)
    println(res2)
    assert(res2 == 9924412)
  }
}
