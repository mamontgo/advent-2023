package mamontgo.advent.day10

import scala.annotation.tailrec

case class PipeMazeSpaces(loop: Seq[Position], others: Seq[Position])

case class Nodes(escapes: Seq[Position], unknown: Seq[Position]) {
  def addEscape(p: Position): Nodes = {
    Nodes(p +: escapes, unknown)
  }

  def addUnknown(p: Position): Nodes = {
    Nodes(escapes, p +: unknown)
  }

}

object Nodes {
  val EMPTY = Nodes(Seq(), Seq())
}

object PipeMazeSpaces {

  def escapeNodes(maze: PipeMaze): Nodes = {
    reduceNodes(
      initNodes(spaces(maze))
    )
  }
  def initNodes(x: PipeMazeSpaces): Nodes = {
    x.others.foldLeft(Nodes.EMPTY) { (i, v) => {
      if (v.col == 0 || v.row == 0) {
        i.addEscape(v)
      } else {
        i.addUnknown(v)
      }
    }
    }
  }

  @tailrec
  def reduceNodes(nodes: Nodes): Nodes = {
    val search = nodes.unknown.foldLeft(Nodes(nodes.escapes, Seq())) { (i, v) => {
        if (PipeMaze.getAdjacentPositions(v).exists(p => i.escapes.contains(p)))
          i.addEscape(v)
        else
          i.addUnknown(v)
      }
    }

    if (search.escapes.size == nodes.escapes.size) {
      search
    } else {
      reduceNodes(search)
    }


  }

  def spaces(maze: PipeMaze): PipeMazeSpaces = {
    val l = PathBuilder.findLongestLoopPath(maze).head.map(a => a.position).map(p => Position.min(p))
    val o = PathBuilder.findAllPaths(maze).flatten.map(a => a.position).map(p => Position.min(p)).filter(i => !l.contains(i))
    val g = PipeMaze.findAllPositions(PipeMaze.GROUND, maze.m).map(p => Position.min(p))
    PipeMazeSpaces(l, o ++ g)
  }

}