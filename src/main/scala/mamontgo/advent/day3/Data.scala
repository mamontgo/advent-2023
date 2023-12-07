package mamontgo.advent.day3

case class Data(previous: Option[String], current: Option[String], next: Option[String])

object Data {
  val EMPTY = Data(None, None, None)

  def valueInclude(position: Int, data: Data): Boolean = {
    matchNext(data, position) || data.previous.exists(p => isMatch(p, position)) || data.current.exists(p => isMatch(p, position))
  }

  private def matchNext(data: Data, position: Int): Boolean = {
    data.next.exists(n => isMatch(n, position))
  }

  private def isMatch(p: String, position: Int): Boolean = {
    (if (position > 0) isCharMatch(p.charAt(position - 1)) else false) || isCharMatch(p.charAt(position)) || (if (position + 1 < p.length) isCharMatch(p.charAt(position + 1)) else false)
  }

  private def isCharMatch(c: Char): Boolean = {
    !c.isDigit && c != '.'
  }
}

