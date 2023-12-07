package mamontgo.advent.day3

import scala.io.Source

class Main {

}

object Main extends App {
  run()

  private def run(): Unit = {

    val source = Source.fromFile("./src/main/resources/day3/input.txt")
    val res = RunQuery.runTally(Input.init(source.getLines()))

    print(s"res $res")
  }
}