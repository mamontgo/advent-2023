package mamontgo.advent.day5

import mamontgo.advent.util.Ftils

import scala.annotation.tailrec
import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
case class Seeds(seeds:Seq[Long], maps: Seq[SeedMap]) {

  lazy val l = asSeedList
  def asMap(): Map[String, SeedMap] = maps.map(m => (m.name, m)).toMap

  def mapAll(source: Long): Long = {
    maps.foldLeft(source) {(i, v) => v.mapDest(i)}
  }

  def lowestRanges(): Long = {
    pairs().foldLeft(Long.MaxValue) { (i, v) => (v._1 to v._1 + v._2).foldLeft(i) {(i2, v2) => mapAll(v2).min(i2) }.min(i)}

  }

  def lowestRangesConcurrent(): Long = {
    Await.result(
     pairs().map(p => Future {
          (p._1 to p._1 + p._2).foldLeft(Long.MaxValue) { (i2, v2) => mapAll(v2).min(i2) }
        }).foldLeft(Future {
          Long.MaxValue
        }) { (i, v) => v.flatMap(l => i.map(i1 => l.min(i1))) }
    , Duration.Inf)
  }

  def pairs(): Seq[(Long, Long)] = {
    (seeds.indices by 2).map(p => (l(p), l(p+1)))
  }

  private def asSeedList: Seq[Long] = seeds.toList
  def lowestLocation(): Long = {
    seeds.foldLeft(Long.MaxValue) {(i, v) => mapAll(v).min(i)}
  }
}
case class SeedRange(rangeStart: Long, sourceRangeStart: Long, rangeLength: Long)
case class SeedMap(name: String, ranges: Seq[SeedRange]) {
  def mapDest(in: Long): Long = {
    Ftils.foldWhile[SeedRange,  Option[Long]](ranges, None:Option[Long],
      t => t.isDefined,
      (_, v) => {
        if (in >= v.sourceRangeStart && in <= v.sourceRangeStart + v.rangeLength) {
          Some(v.rangeStart + (in - v.sourceRangeStart))
        } else {
          None
        }
      }
    ).getOrElse(in)
  }
}
object SeedMap {

  def processLines(lines: Seq[String]): Seeds = {
    val seedData = lines.head.split(':').last.trim.split(' ').map(x => x.trim.toLong)
    Seeds(seedData, processMaps(lines.tail, Seq()))
  }

  @tailrec
  private def processMaps(lines: Seq[String], data: Seq[SeedMap] ): Seq[SeedMap] = {
    lines match {
      case h :: t =>
          if( h.trim.contains("map:")) {
            val name = h.trim.split(' ').head
            val rangeData = Ftils.mapWhile(t, (s: String) => s.trim.nonEmpty, (s: String) => {
              val nums = s.trim.split(' ')
                .map(x => x.trim.toLong)
              SeedRange(nums.head, nums.tail.head, nums.last)
            })
            processMaps(rangeData._2, data :+ SeedMap(name, rangeData._1))
          } else {
            processMaps(t, data)
        }
      case _ => data
    }
  }
}
