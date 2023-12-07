package mamontgo.advent.day3

object RunQuery {

  def runTally(i: (Input, Option[Data])): Long = {
    runCapture(i, 0)
  }

  private def runCapture(running: (Input, Option[Data]), tally: Long): Long = {
    running match {
      case (i, Some(data)) =>
        val newTally = data.current.map(d => NumCapture.numbers(d).foldLeft(tally) {(init: Long, c: NumCapture) =>
            Range.inclusive(c.begin, c.end)
              .find (i => Data.valueInclude(i, data))
              .map(_ => init + c.body.toLong)
              .getOrElse(init)
        }).getOrElse(tally)
        runCapture(i.extractNext(data), newTally)
      case _ => tally
    }
  }



}


