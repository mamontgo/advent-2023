package mamontgo.advent.day10

import mamontgo.advent.util.Ftils
import org.scalatest.funsuite.AnyFunSuite

import scala.annotation.tailrec
import scala.io.Source

class Day10Test extends AnyFunSuite {

  test("find start position example 1") {
    val maze: PipeMaze = PipeMaze.processLines(Source.fromFile("./src/main/resources/day10/example.txt").getLines().toSeq) //    println(maze)
    //    println(maze.start())
    //
    //    val s: Seq[Adjacent] = maze.start().adjacent()

    //    maze.start().adjacent().foreach(x => x.print)
    //
    //    maze.start().adjacent().tail.head.next.foreach(x => x.print)

    //    find(s.head, Seq(s.head)).foreach(x => x.print)

    //    find(s.tail.head, Seq(s.tail.head)).foreach(x => x.print)
    //
    //    val res:Seq[Seq[Adjacent]] = s.map(x => find(x))
    //    val z: Seq[(Adjacent, Adjacent)] = res.head.zip(res.last).reverse
    //    val x = Ftils.foldUntilRes[(Adjacent, Adjacent), Int](z, 1, i => i._1.position.value != PipeMaze.START && i._1.position == i._2.position, (i, _) => i+1)

    println(count(maze))
    assert(count(maze) == 4)
  }

  test("find start position example 2") {
    val maze: PipeMaze = PipeMaze.processLines(Source.fromFile("./src/main/resources/day10/example2.txt").getLines().toSeq)

    println(count(maze))
    assert(count(maze) == 8)
  }


  test("find start position solution part 1") {
    val maze: PipeMaze = PipeMaze.processLines(Source.fromFile("./src/main/resources/day10/test.txt").getLines().toSeq)

    println(count(maze))
    assert(count(maze) == 6842)
  }

  def getMaxSize(res: Seq[Seq[Adjacent]]): Int = {
    res.foldLeft(Map[Int, Int]()) { (i, v) => {
      val key = v.size
      val count = i.getOrElse(key, 0)
      val x: Map[Int, Int] = i.removed(key)
      x + (key -> (count + 1))
    }
    }.filter(i => i._2 == 2).keys.toSeq.sorted.head
  }


  def count(maze: PipeMaze): Int = {
    val mapRes: Seq[Seq[Adjacent]] = maze.start().adjacent().map(x => find(x))

    val max = getMaxSize(mapRes)

    val res = mapRes.filter(p => p.size == max)

    val z: Seq[(Adjacent, Adjacent)] = res.head.zip(res.last).reverse
    Ftils.foldUntilRes[(Adjacent, Adjacent), Int](z, 1, i => i._1.position.value != PipeMaze.START && i._1.position == i._2.position, (i, _) => i + 1)
  }


  def find(a: Adjacent): Seq[Adjacent] = {
    find(a, Seq(a))
  }

  @tailrec final def find(a: Adjacent, result: Seq[Adjacent]): Seq[Adjacent] = {
    val nav = a.navigate()
    if (nav.isEmpty) {
      result
    } else {
      val res = Adjacent(a.m, nav.get, a.position)
      find(res, res +: result)
    }

  }
}
