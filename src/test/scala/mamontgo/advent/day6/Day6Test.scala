package mamontgo.advent.day6

import mamontgo.advent.day6.RaceTimes.distanceCovered
import org.scalatest.funsuite.AnyFunSuite

import scala.io.Source

class Day6Test extends AnyFunSuite {

  test("parse testing") {
    val r: RaceTimes = RaceTimes.processLines(Source.fromFile("./src/main/resources/day6/example.txt").getLines().toSeq)
    println(r)
  }

  test("count winning combos") {
    val r: RaceTimes = RaceTimes.processLines(Source.fromFile("./src/main/resources/day6/example.txt").getLines().toSeq)
    println(r.calc)
    assert(r.calc == 288)
  }

  test("count winning combos solution part1") {
    val r: RaceTimes = RaceTimes.processLines(Source.fromFile("./src/main/resources/day6/test.txt").getLines().toSeq)
    println(r.calc)
    assert(r.calc == 2612736)
  }

  test("count winning combos part2") {
    val r: RaceTimes = RaceTimes.processLines(Source.fromFile("./src/main/resources/day6/test.txt").getLines().toSeq)
    val x = r.calcAll()

    assert(RaceTimes.findMax(0, x.time, x) == RaceTimes.findMax(0, 12994259, x))

  }


  test("count winning combos part2 find lower") {
    val r: RaceTimes = RaceTimes.processLines(Source.fromFile("./src/main/resources/day6/test.txt").getLines().toSeq)
    val x = r.calcAll()


   val limts = RaceTimes.findLimit(RaceTimes.findMax(0, x.time, x), x)

   println(distanceCovered(limts._1+1, x.time))
   println(distanceCovered(limts._2-1, x.time))


  }

  test("count winning combos part1 using limits") {
    val r: RaceTimes = RaceTimes.processLines(Source.fromFile("./src/main/resources/day6/example.txt").getLines().toSeq)
    println(r.calcAllLimits())
    assert(r.calcAllLimits() == 288)
  }

  test("count winning combos part2 using limits example") {
    val r: RaceTimes = RaceTimes.processLines(Source.fromFile("./src/main/resources/day6/example.txt").getLines().toSeq)
    val x = r.calcAll();
    val res  = RaceTimes.findLimit(RaceTimes.findMax(0, x.time, x), x)
    println(res)
    println(1 + res._2 - res._1)
  }


  test("bespoke troubleshooting test") {
    val r: RaceTimes = RaceTimes.processLines(Source.fromFile("./src/main/resources/day6/example.txt").getLines().toSeq)
    val x = r.times.tail.tail.head

    println(RaceTimes.findLimit(RaceTimes.findMax(0, x.time, x), x))

  }

  test("count winning combos part2 using limits solution") {
    val r: RaceTimes = RaceTimes.processLines(Source.fromFile("./src/main/resources/day6/test.txt").getLines().toSeq)
    val x = r.calcAll();
    val res = RaceTimes.findLimit(RaceTimes.findMax(0, x.time, x), x)
    println(res)
    println(1 + res._2 - res._1)
  }

}
