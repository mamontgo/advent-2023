package mamontgo.advent.day3

case class NumCapture(body: String, begin: Int, end: Int)

object NumCapture {
  def numbers(content: String): Seq[NumCapture] = {
    val res: (Option[NumCapture], Seq[NumCapture]) = content.toCharArray.zipWithIndex.foldLeft((None: Option[NumCapture], Seq(): Seq[NumCapture]))({
      case ((capture, data), (char, index)) =>
        if (char.isDigit) {
          capture.map {
            case NumCapture(b, s, e) => (Some(NumCapture(b + char, s, e)), data)
          }.getOrElse((Some(NumCapture("" + char, index, index)), data))
        } else {
          capture.map {
            case NumCapture(b, s, _) => (None, data :+ NumCapture(b, s, index - 1))
          }.getOrElse((None, data))
        }
    })

    res._1 match {
      case Some(NumCapture(b, s, _)) => res._2 :+ NumCapture(b, s, content.length - 1)
      case _ => res._2
    }
  }
}