package mamontgo.advent.day1


case class StringCollectorWithIntMap(collected: String, candidates: Map[String, Int]) extends Collectable[Char, Int] {

  def isCollected(): Boolean = {
    candidates.keys.exists(k => collected.toLowerCase.endsWith(k.toLowerCase))
  }

  def collect(c: Char): Collectable[Char, Int] = {
    if(!isCollected) {
      newCollector(collected + c)
    } else this
  }

  def getCollected(): Option[Int] = {
    candidates.get(candidates.keys.find(k => collected.toLowerCase.endsWith(k.toLowerCase)).getOrElse(""))
  }

  private def newCollector(c: String): Collectable[Char, Int] = {
    StringCollectorWithIntMap(c, candidates)
  }
}

trait Collectable[T, X] {
  def isCollected(): Boolean
  def collect(x:T): Collectable[T, X]
  def getCollected(): Option[X]
}
