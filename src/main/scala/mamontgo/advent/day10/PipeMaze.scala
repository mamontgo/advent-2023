package mamontgo.advent.day10

import mamontgo.advent.day10.PipeMaze.{DIR, Maze, findAdjacent, findStart}
import mamontgo.advent.util.Ftils

import scala.annotation.tailrec

case class PipeMaze(m: Maze) {
  def start(): Position = findStart(m)
}
object PathBuilder {

  def count(maze: PipeMaze): Int = {
    // find all the paths from the start
    val mapRes: Seq[Seq[Adjacent]] = maze.start().adjacent().map(x => find(x))


    // calculate the longest path in loop formation
    val max: Int = getMaxSizeOfLoopingPath(mapRes)

    // get the longest looping paths
    val res = mapRes.filter(p => p.size == max)

    // reverse the last and count items in the navigation paths until the same point is reached
    val z: Seq[(Adjacent, Adjacent)] = res.head.zip(res.last).reverse
    Ftils.foldUntilRes[(Adjacent, Adjacent), Int](z, 1, i => i._1.position.value != PipeMaze.START && i._1.position == i._2.position) { (i, _) => i + 1
    }
  }


  /**
   * Look at all the paths.  We want 2 paths the same length, and then the longest of those paths
   *
   * @param res
   * @return
   */
  def getMaxSizeOfLoopingPath(res: Seq[Seq[Adjacent]]): Int = {
    Ftils.toMap[Seq[Adjacent], Int, Int](res, 0, v => v.size, _ => 1, (a, b) => a + b).filter(i => i._2 == 2).keys.toSeq.min
  }

  def find(a: Adjacent): Seq[Adjacent] = {
    find(a, Seq(a))
  }

  /**
   * Follow adjacent navigation path until it hits the start or a position
   * that can no longer be navigated
   *
   * @param a adjacent item to navigate from
   * @param result the current sequence of navigation followed
   * @return the complete navigation path
   */
  @tailrec final def find(a: Adjacent, result: Seq[Adjacent]): Seq[Adjacent] = {
    val nav = a.navigate()
    if (nav.isEmpty) {
      result
    } else {
      val res = Adjacent(a.m, nav.get, a.position)
      find(res, res +: result)
    }

  }
}

object PipeMaze {
  type Maze = Vector[Vector[Char]]


  val X = "X"
  val NE = "NE"
  val N = "N"
  val NW = "NW"
  val E = "E"
  val W = "W"
  val S = "S"
  val SE = "SE"
  val SW = "SW"

  val DIR: Seq[Vector[String]] = Vector(Vector(NW, N, NE), Vector(W, X, E), Vector(SW, S, SE))

  val NORTH_SOUTH = '|'
  val EAST_WEST = '-'
  val NORTH_EAST = 'L'
  val NORTH_WEST = 'J'
  val SOUTH_WEST = '7'
  val SOUTH_EAST = 'F'
  val GROUND = '.'
  val START = 'S'

  val DIRECTIONS = Seq(NORTH_SOUTH, EAST_WEST, NORTH_EAST, NORTH_WEST, SOUTH_WEST, SOUTH_EAST)
  def processLines(lines: Seq[String]): PipeMaze = {
    PipeMaze(lines.map(l => l.toVector).toVector)
  }

  def findAdjacent(m: Maze, position: Position): Seq[Adjacent] = {
    Seq(
      (position.row-1, position.col),
      (position.row+1, position.col),
      (position.row, position.col-1),
      (position.row, position.col+1)
    ).filter(i => i._1 >= 0 && i._2 >= 0)
      .filter(i => i._2 < m.head.size && i._1 < m.size)
    .map(i => (m(i._1)(i._2), i)).filter(i => {
      DIRECTIONS.contains(i._1)
    }).map(i => {
        Adjacent(m, Position(m, i._2._1, i._2._2), position)
      })
  }

  def findStart(m: Maze): Position = {
    findPosition(START, m)
  }

  def findPosition(d: Char, m: Maze): Position = {
    Ftils.foldUntil[Vector[Char], Position](m, Position(m, -1, -1), p => p.col >= 0 && p.row >= 0) { (i, v) => {
      val col = Ftils.foldUntil[Char, (Int, Int)](v, (-1, -1), i => i._2 >= 0) { (i, v) => {
        val init = i._1 + 1
        if (v == d) (init, init) else (init, -1)
      }
      }._2

      if (col >= 0) Position(m, i.row + 1, col) else Position(m, i.row + 1, -1)
    }}
  }

}

case class Adjacent(m: Maze, position: Position, adjacent: Position) {
  def dir(): String = {
    DIR(position.row - adjacent.row + 1)(position.col - adjacent.col + 1)
  }


  def print = Position.print(position)

  def asString = Position.asString(position)

  /**
   * Find the next position using the navigational characters
   * @return Option position of next step
   */
  def navigate(): Option[Position] = {
    val dest = (dir(), position.value)
    dest match {
      case (PipeMaze.N, PipeMaze.NORTH_SOUTH) => Some(Position(m, position.row-1, position.col))
      case (PipeMaze.S, PipeMaze.NORTH_SOUTH) => Some(Position(m, position.row+1, position.col))
      case (PipeMaze.E, PipeMaze.EAST_WEST) => Some(Position(m, position.row, position.col+1))
      case (PipeMaze.W, PipeMaze.EAST_WEST) => Some(Position(m, position.row, position.col-1))
      case (PipeMaze.W, PipeMaze.NORTH_EAST) => Some(Position(m, position.row-1, position.col))
      case (_, PipeMaze.NORTH_EAST) => Some(Position(m, position.row, position.col+1))
      case (PipeMaze.E, PipeMaze.NORTH_WEST) => Some(Position(m, position.row-1, position.col))
      case (_, PipeMaze.NORTH_WEST) => Some(Position(m, position.row, position.col-1))
      case (PipeMaze.E, PipeMaze.SOUTH_WEST) => Some(Position(m, position.row+1, position.col))
      case (_, PipeMaze.SOUTH_WEST) => Some(Position(m, position.row, position.col-1))
      case (PipeMaze.N, PipeMaze.SOUTH_EAST) => Some(Position(m, position.row, position.col+1))
      case (_, PipeMaze.SOUTH_EAST) => Some(Position(m, position.row+1, position.col))
      case _ => None
    }
  }
}

case class Position(m: Maze, row: Int, col: Int) {
  lazy val value: Char = m(row)(col)
  def adjacent(): Seq[Adjacent] = findAdjacent(m, this)

  def print = Position.print(this)

  def asString = Position.asString(this)
}

object Position {
  val EMPTY = Position(Vector(), 0, 0)
  def print(a: Position) = println(asString(a))

  def asString(a: Position) = s"row ${a.row} col ${a.col} char ${a.value}"
}