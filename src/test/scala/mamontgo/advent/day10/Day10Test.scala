package mamontgo.advent.day10

import org.scalatest.funsuite.AnyFunSuite

import scala.io.Source

class Day10Test extends AnyFunSuite {

  test("find start position example 1") {
    val maze: PipeMaze = PipeMaze.processLines(Source.fromFile("./src/main/resources/day10/example.txt").getLines().toSeq) //    println(maze)

    println(PathBuilder.count(maze))
    assert(PathBuilder.count(maze) == 4)
  }

  test("find start position example 2") {
    val maze: PipeMaze = PipeMaze.processLines(Source.fromFile("./src/main/resources/day10/example2.txt").getLines().toSeq)

    println(PathBuilder.count(maze))
    assert(PathBuilder.count(maze) == 8)
  }


  test("find start position solution part 1") {
    val maze: PipeMaze = PipeMaze.processLines(Source.fromFile("./src/main/resources/day10/test.txt").getLines().toSeq)

    println(PathBuilder.count(maze))
    assert(PathBuilder.count(maze) == 6842)
  }

}
