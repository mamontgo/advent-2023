package mamontgo.advent.day3

case class NumData(data: Data, previous: Seq[NumCapture], current: Seq[NumCapture], next: Seq[NumCapture])

object NumData {
  def fromData(data: Data) = {
    NumData(data, asSeq(data.previous), asSeq(data.current), asSeq(data.next))
  }

  private def asSeq(data: Option[String]): Seq[NumCapture] = {
    data.map(p => NumCapture.numbers(p)).getOrElse(Seq())
  }

  def getAdjacent(numData: NumData, position: Int): Seq[Long] = {
    val filter = filterMatches(position)
    filter(numData.previous) ++ filter(numData.current) ++ filter(numData.next)
  }

  private val filterMatches: Int =>  Seq[NumCapture] => Seq[Long] = position => nums => {
    nums.filter(n => n.begin <= position + 1 && n.end >= position - 1).map(n => n.body.toLong)
  }

}