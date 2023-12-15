package mamontgo.advent.day8

import mamontgo.advent.day7.{CamelPoker, PrizeDeal}
import mamontgo.advent.util.MyMath
import org.scalatest.funsuite.AnyFunSuite

import scala.annotation.tailrec
import scala.collection.immutable.NumericRange
import scala.io.Source

class Day8Test extends AnyFunSuite {

  test("read example 1") {
    val m: MapDirections = MapDirections.processLines(Source.fromFile("./src/main/resources/day8/example.txt").getLines().toSeq)
    println(m.countSteps("AAA", "ZZZ"))
    println(m)
  }


  test("read example 2") {
    val m: MapDirections = MapDirections.processLines(Source.fromFile("./src/main/resources/day8/example2.txt").getLines().toSeq)
    println(m.countSteps("AAA", "ZZZ"))
    println(m)
  }


  test("part 1 solution") {
    val m: MapDirections = MapDirections.processLines(Source.fromFile("./src/main/resources/day8/test.txt").getLines().toSeq)
    println(m.countSteps("AAA", "ZZZ"))
    println(m)
  }

  test("part 2 all node ending with Z example 3") {
    val m: MapDirections = MapDirections.processLines(Source.fromFile("./src/main/resources/day8/example3.txt").getLines().toSeq)
    println(m.countAllSteps())
    println(m)
    assert(m.countAllSteps() == 6)
  }

  test("part 2 all node ending with Z solution") {
    val m: MapDirections = MapDirections.processLines(Source.fromFile("./src/main/resources/day8/test.txt").getLines().toSeq)
    println(m.countAllSteps())
    println(m)
    assert(m.countAllSteps() == 6)
  }


  test("initial multithreaded solution test") {
    val m: MapDirections = MapDirections.processLines(Source.fromFile("./src/main/resources/day8/test.txt").getLines().toSeq)
    m.concurrentStepAll()
  }

  test("get x values") {
    val m: MapDirections = MapDirections.processLines(Source.fromFile("./src/main/resources/day8/example3.txt").getLines().toSeq)
    var h: ItemSearchCollect = m.listSearchCollect.head

    (1 to 1000).foreach { _ =>
      h = m.findItems(h)
    }
    println(h.matchPositions)
    h = m.listSearchCollect.tail.head
    (1 to 1000).foreach { _ =>
      h = m.findItems(h)
    }

    println(h.matchPositions)
    println((-1 to 10000000 by 15517).toList.tail)

  }

  test("get x values second") {
    val m: MapDirections = MapDirections.processLines(Source.fromFile("./src/main/resources/day8/test.txt").getLines().toSeq)
    var h: ItemSearchCollect = m.listSearchCollect.tail.head

    (1 to 10000).foreach { _ =>
      h = m.findItems(h)
    }

    println(h.matchPositions)
    println((-1 to 10000000 by 11309).toList.tail)

  }



  test("get x values pattern match") {
    val m: MapDirections = MapDirections.processLines(Source.fromFile("./src/main/resources/day8/test.txt").getLines().toSeq)
    val h = m.listSearchCollect.map(i => m.searchAll(ItemSearchAll(Seq(i.next), i.testResult, i.steps, found = false)).steps)

    println(s"LCD if repeating patterns: ${MyMath.lcm(h.map(l => BigInt(l)))}")


  }
}
