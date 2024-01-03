package mamontgo.advent.day10

import mamontgo.advent.util.Ftils
import org.scalatest.funsuite.AnyFunSuite

import scala.io.Source

class Day10SpacesTest extends AnyFunSuite {

  test("check spaces calc") {
    val maze: PipeMaze = PipeMaze.processLines(Source.fromFile("./src/main/resources/day10/example.txt").getLines().toSeq) //    println(maze)
    val e = PipeMazeSpaces.escapeNodes(maze)
    assert(e.unknown.size == 1)
  }

  test("check escapes example 1") {
    val maze: PipeMaze = PipeMaze.processLines(Source.fromFile("./src/main/resources/day10/escapes1.txt").getLines().toSeq) //    println(maze)
    val e = PipeMazeSpaces.escapeNodes(maze)
    assert(e.unknown.size == 4)
  }

  test("check escapes example 2") {
    val maze: PipeMaze = PipeMaze.processLines(Source.fromFile("./src/main/resources/day10/escapes2.txt").getLines().toSeq) //    println(maze)
    val e = PipeMazeSpaces.escapeNodes(maze)
    assert(e.unknown.size == 4)
  }

}
