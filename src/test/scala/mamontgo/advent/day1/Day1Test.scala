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


  test("Read file and test any text digit total") {

    val source = Source.fromFile("./src/main/resources/day1/part2.txt")
    val res = Calibrate.addAny(source.getLines())
    assert(res == 281)
  }

  test("Read file and test any text digit total real") {

    val source = Source.fromFile("./src/main/resources/day1/part2_real.txt")
    val res = Calibrate.addAny(source.getLines())
    assert(res == 53221)
  }


  test("get first number") {
    assert(Calibrate.findFirstAnyNumber("helloone1here").getOrElse(0) == 1)
    assert(Calibrate.findFirstAnyNumber("helloone2here").getOrElse(0) == 1)
    assert(Calibrate.findFirstAnyNumber("hell2oone2here").getOrElse(0) == 2)
    assert(Calibrate.findFirstAnyNumber("two").getOrElse(0) == 2)
    assert(Calibrate.findFirstAnyNumber("2").getOrElse(0) == 2)
  }

  test("get last number") {
//    println(Calibrate.findLastAnyNumber("helloone1hereonehere"))
//    println(Calibrate.findLastAnyNumber("helloone2hereonehere"))
//    println(Calibrate.findLastAnyNumber("helloone2hereone3here"))
//    assert(Calibrate.findFirstAnyNumber("helloone1here").getOrElse(0) == 1)

    assert(Calibrate.findLastAnyNumber("helloone1here").getOrElse(0) == 1)
    assert(Calibrate.findLastAnyNumber("helloone2here").getOrElse(0) == 2)
    assert(Calibrate.findLastAnyNumber("helloone2threehere").getOrElse(0) == 3)

  }

}
