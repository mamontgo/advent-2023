package mamontgo.advent.day1

import org.scalatest.funsuite.AnyFunSuite

import scala.io.Source

class Day1Test extends AnyFunSuite {

  test("Read file and test total") {

    val source = Source.fromFile("./src/main/resources/day1/example.txt")
    val res = Calibrate.add(source.getLines())
    assert(res == 142)
  }


  test("Read file and test total real") {

    val source = Source.fromFile("./src/main/resources/day1/calib.txt")
    val res = Calibrate.add(source.getLines())
    assert(res == 55834)
  }


  test("get first number") {
    println(Calibrate.findFirstAnyNumber("helloone1here"))

    assert(Calibrate.findFirstAnyNumber("helloone1here").getOrElse(0) == 1)
  }

  test("get last number") {
//    println(Calibrate.findLastAnyNumber("helloone1hereonehere"))
    println(Calibrate.findLastAnyNumber("helloone2hereonehere"))
//    println(Calibrate.findLastAnyNumber("helloone2hereone3here"))
//    assert(Calibrate.findFirstAnyNumber("helloone1here").getOrElse(0) == 1)
  }

}
