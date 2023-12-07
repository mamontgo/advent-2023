package mamontgo.advent.day3

import org.scalatest.funsuite.AnyFunSuite

import scala.io.Source

class MainTest  extends AnyFunSuite {

  test("Read file and test total") {

    val source = Source.fromFile("./src/main/resources/day3/input.txt")
    val res = RunQuery.runTally(Input.init(source.getLines()))

    assert(res == 554003)
  }

  test("Read file and test gears total") {

    val source = Source.fromFile("./src/main/resources/day3/test_gears.txt")
    val input = Input.initNum(source.getLines())
    val res = GearQuery.runTally(input)

    assert(res == 39483)
  }

  test("Read file and test gears total real") {

    val source = Source.fromFile("./src/main/resources/day3/gears.txt")
    val input = Input.initNum(source.getLines())
    val res = GearQuery.runTally(input)

    assert(res == 87263515)
  }
}
