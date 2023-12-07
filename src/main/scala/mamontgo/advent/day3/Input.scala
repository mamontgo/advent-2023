package mamontgo.advent.day3

case class Input (source: Iterator[String])  {
    def extractNext(d: Data): (Input, Option[Data]) = {
        d match {
            case Data(None, None, None) => (Input(source), Some(Data(None, next, next)))
            case Data(_, _, None) => (Input(source), None)
            case Data(_, c, n) => (Input(source), Some(Data(c, n, next)))
            case _ => (Input(source), None)
        }
    }

    def extractNum(d: Data): (Input, Option[NumData]) = {
        val x = extractNext(d)
        (x._1, x._2.map(d => NumData.fromData(d)))
    }

    private def next: Option[String] = if(source.hasNext) Some(source.next()) else None
}

object Input {
    def init(source: Iterator[String]): (Input, Option[Data]) = Input(source).extractNext(Data.EMPTY)
    def initNum(source: Iterator[String]): (Input, Option[NumData]) = Input(source).extractNum(Data.EMPTY)
}