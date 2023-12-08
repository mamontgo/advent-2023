package mamontgo.advent.day2.model
case class Game(id: Int, set: Set[Map[String, Int]]) {

}

object Game {

  val colors = List("red", "green", "blue")
  def fromString(data: String): Game = {
    val header = data.split(':')
    val sets = header.tail.head.split(';')

    Game(header.head.replace("Game ", "").toInt,
      sets.map(s => s.split(',')
        .map(c => c.trim.split(' '))
        .map(data => (data.last.toLowerCase, data.head.toInt)).toMap
      ).toSet)

  }

  def filter(games: Seq[Game], counts: Seq[(String, Int)]): Seq[Int] = {
    games.filter(
      game => {
        counts.foldLeft(true)((init, n) => {
          game.set.map(m => m.getOrElse(n._1, 0)).sum <= n._2 && init
        })
      }).map(game => game.id)
  }

  def filterIndividually(games: Seq[Game], counts: Seq[(String, Int)]): Seq[Int] = {
    games.filter(
      game => {
        counts.foldLeft(true)((init, n) => {
          game.set.foldLeft(init) ((i, m) => m.getOrElse(n._1, 0) <= n._2 && i)
        })
      }).map(game => game.id)
  }

  def minPowerSet(games: Seq[Game]): Seq[Long] = {
    games.map(game => {
      colors.map(c => {
        game.set.foldLeft(0) ((i, v) => v.get(c).getOrElse(0).max(i))
      }).product
    })
  }
  def summary(games: Seq[Game]): Unit = {
    games.map(game => {
      print("Game ")
      print(game.id)
      print(": ")
      colors.foreach(c => {
        printf(s" $c ${game.set.map(m => m.getOrElse(c, 0)).sum}")
      })
      println
    })
  }
}

