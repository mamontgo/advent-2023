package mamontgo.advent.day1

import mamontgo.advent.util.Ftils

import scala.annotation.tailrec

object Calibrate {

  private type StringCollectable = Collectable[Char, Int]
  private type StringCollectableResult = (StringCollectable, Option[Int])

  val numbers: Map[String, Int] = Map("zero" -> 0, "one" -> 1, "two" -> 2, "three" -> 3, "four" -> 4, "five" -> 5, "six" -> 6, "seven" -> 7, "eight" -> 8, "nine" -> 9)
  val init: StringCollectableResult = (StringCollectorWithIntMap("", numbers), None)

  private val numbersReverse: Map[String, Int] = numbers.map(e => (e._1.reverse, e._2))
  val initReverse: StringCollectableResult = (StringCollectorWithIntMap("", numbersReverse), None)

  val collectionCompleteTest: StringCollectableResult => Boolean = in => in._2.isDefined

  val collect: (StringCollectableResult, Char) => StringCollectableResult =  (in, c) => {
    if (c.isDigit) {
      (in._1, Some(c.asDigit))
    }
    else {
      val n = in._1.collect(c)
      if (n.isCollected()) {
        (n, n.getCollected())
      }
      else (n, in._2)
    }
  }

  def findFirst(data: String): Char = data.find(c => c.isDigit).getOrElse('0')

  def findLast(data: String): Char = data.findLast(c => c.isDigit).getOrElse('0')

  def find(data: String): Long = ("" + findFirst(data) + findLast(data)).toLong

  def findAny(data: String): Long = ("" + findFirstAnyNumber(data).getOrElse(0) + findLastAnyNumber(data).getOrElse(0)).toLong

  def add(source: Iterator[String]): Long = {
    source.map(s => find(s)).sum
  }

  def addAny(source: Iterator[String]): Long = {
    source.map(s => findAny(s)).sum
  }
  def findFirstAnyNumber(data: String): Option[Int] = {
    Ftils.foldWhile[Char, (Collectable[Char, Int], Option[Int])](
      data.toCharArray,
      init,
      collectionCompleteTest,
      collect
    )._2
  }

  def findLastAnyNumber(data: String): Option[Int] = {
    Ftils.foldWhile[Char, (Collectable[Char, Int], Option[Int])](
      data.reverse.toCharArray,
      initReverse,
      collectionCompleteTest,
      collect
    )._2
  }





}
