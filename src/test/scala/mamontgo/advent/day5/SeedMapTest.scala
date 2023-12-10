package mamontgo.advent.day5

import org.scalatest.funsuite.AnyFunSuite

import scala.io.Source
import scala.util.Using

class SeedMapTest extends AnyFunSuite {

  test("input test") {
    Using(Source.fromFile("./src/main/resources/day5/example.txt")) { s =>
      val source = s.getLines().toSeq
      val seeds = SeedMap.processLines(source)
      println(seeds)
    }
  }

  test("input test soil numbers") {
    Using(Source.fromFile("./src/main/resources/day5/example.txt")) { s =>
      val source = s.getLines().toSeq
      val seeds: Seeds = SeedMap.processLines(source)

      assertSeed(seeds, 79, 81)
      assertSeed(seeds, 14, 14)
      assertSeed(seeds, 55, 57)
      assertSeed(seeds, 13, 13)

    }

  }

  test("test full seed mapping") {
    Using(Source.fromFile("./src/main/resources/day5/example.txt")) { s =>
      val source = s.getLines().toSeq
      val seeds: Seeds = SeedMap.processLines(source)
      println(seeds.mapAll(79))

      assert(seeds.mapAll(79) == 82)
      assert(seeds.mapAll(14) == 43)
      assert(seeds.mapAll(55) == 86)
      assert(seeds.mapAll(13) == 35)

      assert(seeds.lowestLocation() == 35)
    }
  }

  test("test full seed mapping solution") {
    Using(Source.fromFile("./src/main/resources/day5/test.txt")) { s =>
      val source = s.getLines().toSeq
      val seeds: Seeds = SeedMap.processLines(source)
      println(seeds.lowestLocation())

      assert(seeds.lowestLocation() == 388071289)
    }
  }

  def assertSeed(seeds: Seeds, source: Int, expected: Int): Unit = {
    assert(seeds.asMap().get("seed-to-soil").map(m => {
      m.mapDest(source)
    }).getOrElse(0) == expected)
  }

  test("test full seed pairs") {
    val seeds: Seeds = SeedMap.processLines(Source.fromFile("./src/main/resources/day5/example.txt").getLines().toSeq)
    println(seeds.pairs())
    assert(seeds.pairs().head == (79, 14))
    assert(seeds.pairs().last == (55, 13))

  }

  test("test full seed pairs lowest range") {
    val seeds: Seeds = SeedMap.processLines(Source.fromFile("./src/main/resources/day5/example.txt").getLines().toSeq)
    println(seeds.lowestRanges())
    assert(seeds.lowestRanges() == 46)
  }

  test("test full seed pairs lowest solution") {
    val seeds: Seeds = SeedMap.processLines(Source.fromFile("./src/main/resources/day5/test.txt").getLines().toSeq)
    println(seeds.lowestRanges())
    assert(seeds.lowestRanges() == 46)
  }


  test("test full seed pairs lowest solution concurrent demo") {
    val seeds: Seeds = SeedMap.processLines(Source.fromFile("./src/main/resources/day5/example.txt").getLines().toSeq)
    println(s"Res ${seeds.lowestRangesConcurrent()}")

  }

  test("test full seed pairs lowest solution concurrent") {
    val seeds: Seeds = SeedMap.processLines(Source.fromFile("./src/main/resources/day5/test.txt").getLines().toSeq)
//    println(s"Res ${seeds.lowestRangesConcurrent()}")
    //84206669
  }

  test("step test") {
    val s = 0
    val e = 10
    val r = (s to e by 4)
    r.foreach(x => println(s"val $x"))

  }
}
