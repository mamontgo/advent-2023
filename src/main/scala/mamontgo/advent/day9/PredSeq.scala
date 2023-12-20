package mamontgo.advent.day9

import scala.annotation.tailrec
object PredSeq {

  def processLines(lines: Seq[String]): Seq[Seq[Long]] = {
    lines.map(l => l.split(' ').map(x => x.toLong))
  }

  def totalAll(x: Seq[Seq[Long]]): Long = {
    x.map(v => calcTotal(v)).sum
  }

  def totalAllStart(x: Seq[Seq[Long]]): Long = {
    x.map(v => calcTotalStart(v)).sum
  }
  def calcTotal(x: Seq[Long]): Long = {
    extrapolate(x).map(l => l.reverse).foldLeft(0L) { (i, v) => v.head + i }
  }
  def calcTotalStart(x: Seq[Long]): Long = {
    extrapolate(x).foldLeft(0L) { (i, v) => v.head - i }
  }

  def extrapolate(x: Seq[Long]): Seq[Seq[Long]] = {
    extrapolateLine(Seq(x))
  }
  @tailrec
  private def extrapolateLine(x: Seq[Seq[Long]]): Seq[Seq[Long]] = {
    if(x.head.forall(x => x == 0)) {
      x
    } else {
      extrapolateLine(lineDiff(x.head) +: x)
    }
  }
  private def lineDiff(x: Seq[Long]): Seq[Long] = {
    x.tail.foldLeft((x.head, Seq[Long]())) { (i, v) => (v, i._2 :+ (v-i._1)) }._2
  }
}
