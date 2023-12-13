package mamontgo.advent.day6

import mamontgo.advent.day6.RaceTimes.{findLimit, findMax}

import scala.annotation.tailrec


case class RaceTimes(times: Seq[RaceTime]){
  def calc = times.map(r => {
    r.countCombos()
  }).product

  def calcAll():RaceTime = {
    val s = times.foldLeft(("", "")) {
      (i, v) => (i._1 + v.time, i._2 + v.distance)
    }
    RaceTime(s._1.toLong, s._2.toLong)
  }

  def calcAllLimits(): Long = {
    times.map(
      r => findLimit(findMax(0, r.time, r), r)
    ).map(t => 1 + (t._2 - t._1)).product

  }

  def calcSingleLimits(): Long = {
    val x = calcAll()
    val t = findLimit(findMax(0, x.time, x), x)
    1 + (t._2 - t._1)
  }
}

case class RaceTime(time: Long, distance: Long) {
  def countCombos(): Long = {
    Range.Long.inclusive(1L , distance - 1, 1).foldLeft(0L) {(i, h) =>
      if(RaceTimes.distanceCovered(h, time) > distance) i+1 else i
    }
  }
}
object RaceTimes {

  private val SPEED_PER_SEC_PRESS = 1;

  def distanceCovered(pressTime: Long, desiredTime: Long): Long = {
    (pressTime * RaceTimes.SPEED_PER_SEC_PRESS) * (desiredTime - pressTime)
  }

  def processLines(lines: Seq[String]): RaceTimes = {
    val timeData:Seq[Long] = lines.head.split(':').last.trim.replaceAll("  ", " ")
      .split(' ')
      .filter(s => s.nonEmpty && s.forall(c => c.isDigit))
      .map(x => x.trim.toLong)
    val distanceData:Seq[Long] = lines.tail.head.split(':').last.trim.replaceAll("  ", " ")
      .split(' ')
      .filter(s => s.nonEmpty && s.forall(c => c.isDigit))
      .map(x => x.trim.toLong)
    RaceTimes(timeData.zip(distanceData).map(t => RaceTime(t._1, t._2)))
  }

  @tailrec
  def findMax(min: Long, max: Long, race: RaceTime): Long = {

    val m: Long = min + ((max - min)/2)

    val mr = distanceCovered(m, race.time)
    val lr = distanceCovered(min, race.time)
    val ur = distanceCovered(max, race.time)

//    println(s"Locate Max = RACE: $race  |||  m: $m -> $mr, u: $max -> $ur, l: $min -> $lr")

    if(lr == ur && lr == mr) {
      println(s"MAX VALUE = $m")
      return m
    }

    if (ur > mr) {
      // upper limit becomes new middle
      findMax(m, max + (m - min).min(race.time), race)
    } else if (lr > mr) {
      // lower limit becomes new middle
      findMax(min - (m - min).max(1), m, race)
    } else {
      // narrow bands around middle
      val newBound = (m - min) / 2
      findMax(m - newBound, m + newBound, race)
    }

  }

  def findLimit(max: Long, race: RaceTime): (Long, Long) = {
    if (distanceCovered(max, race.time) < race.distance) {
      (0, 0)
    } else {
      (findLower(0, max, race), findUpper(max, race.time, race))
    }
  }

  @tailrec
  def findLower(min: Long, max: Long, race: RaceTime): Long = {
    val minR = distanceCovered(min, race.time)

//    println(s"Find :Lower = RACE: $race  |||  max: $max -> $maxR, min: $min -> $minR")
    if(min == max) {
      if(minR > race.distance) {
        println(s"Find :Lower = RACE: $race  |||  FOUND= $min ")
        min
      } else {
        findLower(min+1, max+1, race)
      }
    }  else {
      if (minR >= race.distance) {
        // shift min to minR
        findLower((min - (max - min)).max(0), min, race)
      } else {
        val p = Math.ceil((max - min).toFloat / 2)
        val updated = min + p.toLong
        findLower(updated, max, race)
      }
    }
  }

  @tailrec
  def findUpper(min: Long, max: Long, race: RaceTime): Long = {
    val maxR = distanceCovered(max, race.time)

//    println(s"Find :Upper = RACE: $race  |||  max: $max -> $maxR, min: $min -> $minR")
    if (min == max) {
      if(maxR <= race.distance) {
        findUpper(max-1, max-1, race)
      } else {
        println(s"Find :Upper = RACE: $race  |||  FOUND= $min ")
        min
      }
    } else {
      if (maxR >= race.distance) {
        // shift max to minR
        findUpper(max, (max + (max - min)).max(race.time),race)
      } else {
        val p = Math.ceil((max - min).toFloat / 2)
        val updated = max - p.toLong
        findUpper(min, updated, race)
      }
    }
  }

}
