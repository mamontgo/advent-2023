package mamontgo.advent.day2

import mamontgo.advent.day2.model.Game
import org.scalatest.funsuite.AnyFunSuite

import scala.io.Source

class Day2Tests extends AnyFunSuite {

  test("test line parser") {
    val game = Game.fromString("Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green")
    assert(game.set.head.getOrElse("blue", 0) == 3)
  }

  test("filtering") {
    val source = Source.fromFile("./src/main/resources/day2/example.txt")
    val games = source.getLines().map(l => Game.fromString(l))
    assert(games.length == 5)
  }

  test("filtering ids") {
    val source = Source.fromFile("./src/main/resources/day2/example.txt")
    val games = source.getLines().map(l => Game.fromString(l))
    val init: List[(String, Int)] = List(("red", 12), ("blue", 14), ("green", 13))
    val res = Game.filter(games.toSeq, init)
    assert(res.sum == 8)
  }


  test("filtering ids individually") {
    val source = Source.fromFile("./src/main/resources/day2/example.txt")
    val games = source.getLines().map(l => Game.fromString(l))
    val init: List[(String, Int)] = List(("red", 12), ("blue", 14), ("green", 13))
    val res = Game.filterIndividually(games.toSeq, init)
    assert(res.sum == 8)
  }

  test("filtering ids test") {
    val source = Source.fromFile("./src/main/resources/day2/test.txt")
    val games = source.getLines().map(l => Game.fromString(l)).toList
    val init: List[(String, Int)] = List(("red", 12), ("green", 13), ("blue", 14))
    val res = Game.filter(games, init)
    assert(res.sum == 486)
  }

  test("filtering ids test individually") {
    val source = Source.fromFile("./src/main/resources/day2/test.txt")
    val games = source.getLines().map(l => Game.fromString(l)).toList
    val init: List[(String, Int)] = List(("red", 12), ("green", 13), ("blue", 14))
    val res = Game.filterIndividually(games, init)
    assert(res.sum == 2771)
  }


  test("power set  ids") {
    val source = Source.fromFile("./src/main/resources/day2/example.txt")
    val games = source.getLines().map(l => Game.fromString(l))

    val res = Game.minPowerSet(games.toSeq)
    println(res)
    println(res.sum)
    assert(res.sum == 2286)
  }

  test("power set ids test") {
    val source = Source.fromFile("./src/main/resources/day2/test.txt")
    val games = source.getLines().map(l => Game.fromString(l))

    val res = Game.minPowerSet(games.toSeq)
    println(res)
    println(res.sum)
    assert(res.sum == 70924)
  }
}
