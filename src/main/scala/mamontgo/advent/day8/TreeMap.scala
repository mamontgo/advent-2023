package mamontgo.advent.day8

import mamontgo.advent.util.Ftils

import java.util.Date
import scala.annotation.tailrec
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}

sealed abstract class Dir(d: String)

object Dir {
  case object L extends Dir("LEFT")

  case object R extends Dir("RIGHT")

}


case class MapDirections(dirs: Seq[Dir], map: TreeMap) {

  private val start = new Date().getTime

  def get(key: String): Node = map.get(key)

  lazy val startItems: Seq[String] = map.nodes.keys.filter(k => k.endsWith("A")).toSeq

  lazy val listSearchCollect: Seq[ItemSearchCollect] = startItems.map(a => ItemSearchCollect(a, MapDirections.endWithZTest, 0, Seq()))

  def concurrentStepAll(): Long = {
    testConcurrent(0, listSearchCollect, Seq())
  }

  @tailrec final def testConcurrent(iteration: Long, list: Seq[ItemSearchCollect], matches: Seq[Seq[Long]]): Long = {
    val res: Seq[Any] = Await.result(Future.sequence(Seq(processConcurrent(list), Future {
      MapDirections.matches(matches)
    })), Duration.Inf)

    val result = res.tail.head.asInstanceOf[Option[Long]]
    val output = res.head.asInstanceOf[Seq[ItemSearchCollect]]
    if (result.isDefined) {
      result.get
    } else {
      if (iteration % 10 == 0) {
        println(s"Step ${output.head.steps} seconds running: ${(new Date().getTime - start) / 1000}")
      }
      testConcurrent(iteration + 1, output.map(i => ItemSearchCollect(i.next, i.testResult, i.steps, Seq())), output.map(i => i.matchPositions))
    }

  }

  def processConcurrent(list: Seq[ItemSearchCollect]): Future[Seq[ItemSearchCollect]] = {
    Future.sequence(list.map(i => Future {
      breakingSearch(100000L, i)
    }))

  }

  def countAllSteps(): Long = {
    searchAll(ItemSearchAll(startItems, MapDirections.endWithZTest, 0, found = false)).steps
  }

  def countSteps(start: String, dest: String): Long = {
    search(ItemSearch.init(start, dest)).steps
  }

  @tailrec final def searchAll(s: ItemSearchAll): ItemSearchAll = {
    val res: ItemSearchAll = Ftils.foldUntil[Dir, ItemSearchAll](dirs, s, i => i.found) { (i, v: Dir) =>
      i.test(i.next.map { key => get(key).get(v) })
    }

    if (s.steps % 100000l == 0) {
      println(s"step: ${s.steps} current: ${s.next} seconds running: ${(new Date().getTime - start) / 1000}")
    }

    if (res.found) {
      res
    } else {
      searchAll(res)
    }
  }


  @tailrec private final def search(s: ItemSearch): ItemSearch = {
    val res = searchFold(s)
    if (res.found) {
      res
    } else {
      search(res)
    }
  }

  @tailrec final def breakingSearch(attempts: Long, s: ItemSearchCollect): ItemSearchCollect = {
    val res = findItems(s)
    if (attempts == 0) {
      res
    } else {
      breakingSearch(attempts - 1, res)
    }
  }

  def findItems(s: ItemSearchCollect): ItemSearchCollect = {
    dirs.foldLeft(s) { (i, v) =>
      i.test(map.get(i.next).get(v))
    }
  }

  private def searchFold(s: ItemSearch): ItemSearch = {
    Ftils.foldUntil[Dir, ItemSearch](dirs, s, i => i.found) { (i, v) =>
      i.test(map.get(i.next).get(v))
    }
  }

}

case class ItemSearchAll(next: Seq[String], test: String => Boolean, steps: Long, found: Boolean) {
  def test(updatedNext: Seq[String]): ItemSearchAll = {
    if (updatedNext.forall(v => test(v))) {
      foundNext(updatedNext)
    } else {
      inc(updatedNext)
    }
  }

  def foundNext(updatedNext: Seq[String]) = ItemSearchAll(updatedNext, test, steps + 1, found = true)

  def inc(updatedNext: Seq[String]) = ItemSearchAll(updatedNext, test, steps + 1, found)
}

case class ItemSearchCollect(next: String, testResult: String => Boolean, steps: Long, matchPositions: Seq[Long]) {

  def test(updatedNext: String): ItemSearchCollect = {
    ItemSearchCollect(updatedNext, testResult: String => Boolean, steps + 1, if (testResult(updatedNext)) matchPositions :+ steps else matchPositions)
  }
}

case class ItemSearch(next: String, dest: String, steps: Long, found: Boolean) {

  def test(updatedNext: String) = {
    if (updatedNext == dest) {
      found(updatedNext)
    } else {
      inc(updatedNext)
    }
  }

  def inc(updatedNext: String) = ItemSearch(updatedNext, dest, steps + 1, found)

  def found(updatedNext: String) = ItemSearch(updatedNext, dest, steps + 1, found = true)
}

object ItemSearch {
  def init(first: String, dest: String): ItemSearch = {
    ItemSearch(first, dest, 0L, found = false)
  }
}

case class TreeMap(nodes: Map[String, Node]) {

  def get(name: String) = nodes.getOrElse(name, Node.EMPTY)

  def add(nodeName: String, node: Node): TreeMap = {
    val x = (nodeName, node)
    TreeMap(nodes + x)
  }

}

case class Node(name: String, left: String, right: String) {
  def get(d: Dir): String = if (d == Dir.L) left else right
}

object Node {
  def parse(i: String): Node = {
    val parts = i.split('=')
    val nodes = parts(1).trim.replace("(", "").replace(")", "").split(',')
    Node(parts(0).trim, nodes(0).trim, nodes(1).trim)
  }

  lazy val EMPTY = Node("", "", "")
}

object MapDirections {

  def matches(res: Seq[Seq[Long]]): Option[Long] = {
    if (res.isEmpty)
      None
    else
      Ftils.foldUntil[Long, Option[Long]](res.head, None: Option[Long], x => x.isDefined) {
        (_, v) => if (res.tail.forall(v1 => v1.contains(v))) Some(v) else None
      }
  }

  val endWithZTest: String => Boolean = x => x.endsWith("Z")

  def processLines(lines: Seq[String]): MapDirections = {
    val dirs: Seq[Dir] = lines.head.map(d => if (d == 'L') Dir.L else Dir.R)
    val x = lines.tail.foldLeft(TreeMap.EMPTY) { (i, v) =>
      if (v.nonEmpty) {
        val node = Node.parse(v)
        i.add(node.name, node)
      } else {
        i
      }
    }
    MapDirections(dirs, x)
  }
}

object TreeMap {
  lazy val EMPTY = TreeMap(Map())
}

