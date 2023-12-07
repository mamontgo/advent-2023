package mamontgo.advent.day3

object GearQuery {

  def runTally(i: (Input, Option[NumData])): Long = {
    runTally(i, 0)
  }

  def runTally(i: (Input, Option[NumData]), tally: Long): Long = {
    i match {
      case (_, Some(nd)) => nd.data.current.map(s => s.toCharArray.zipWithIndex.foldLeft(tally) {
        case (init, (value, index)) =>
          if (value == '*') {
            val nums: Seq[Long] = NumData.getAdjacent(nd, index)
            if (nums.length == 2) nums.product + init else init
          } else init
        case _ => tally

      }).map(newTally => i._2.map(n => n.data).map(d => runTally(i._1.extractNum(d), newTally)).getOrElse(newTally)).getOrElse(tally)
      case _ => tally
    }

  }

}
