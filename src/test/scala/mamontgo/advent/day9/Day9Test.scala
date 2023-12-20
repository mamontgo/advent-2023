package mamontgo.advent.day9

import org.scalatest.funsuite.AnyFunSuite

import scala.io.Source

class Day9Test extends AnyFunSuite {

  test("negative numbers") {
   println(PredSeq.extrapolate(PredSeq.processLines(Seq("-7 -3 9 42")).head))
   println(PredSeq.extrapolate(PredSeq.processLines(Seq("-7 -3 9 42 129 327 726 1476 2850 5366 9996 18495 33888 61158 108183 186975 315279 518595 832691 1306680 2006739")).head))
  }

  test("extrapolate number seq") {
    val r: Seq[Seq[Long]] = PredSeq.processLines(Source.fromFile("./src/main/resources/day9/example.txt").getLines().toSeq)
    println(r.map(l => PredSeq.extrapolate(l)))
  }

  test("Part 1 example result") {
    val r: Seq[Seq[Long]] = PredSeq.processLines(Source.fromFile("./src/main/resources/day9/example.txt").getLines().toSeq)
    println(PredSeq.totalAll(r))
    assert(PredSeq.totalAll(r)  == 114)
  }

  test("Part 1 troubleshooting ") {
    val r: Seq[Seq[Long]] = PredSeq.processLines(Source.fromFile("./src/main/resources/day9/test.txt").getLines().toSeq)
    r.map(r => PredSeq.extrapolate(r).head).foreach(r => println(r))
  }
  test("Part 1 test result") {
    val r: Seq[Seq[Long]] = PredSeq.processLines(Source.fromFile("./src/main/resources/day9/test.txt").getLines().toSeq)

    assert(PredSeq.totalAll(r)  == 1842168671)
    println(s"Result: ${PredSeq.totalAll(r)}")
  }


  test("Part 2 example result") {
    val r: Seq[Seq[Long]] = PredSeq.processLines(Source.fromFile("./src/main/resources/day9/example.txt").getLines().toSeq)
    assert(PredSeq.totalAll(r)  == 114)

    assert(PredSeq.totalAllStart(r) == 2)
  }


  test("Part 2 solution result") {
    val r: Seq[Seq[Long]] = PredSeq.processLines(Source.fromFile("./src/main/resources/day9/test.txt").getLines().toSeq)

    println(s"Result: ${PredSeq.totalAllStart(r)}")
    assert(PredSeq.totalAllStart(r) == 903)
  }

}
