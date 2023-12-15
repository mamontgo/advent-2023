package mamontgo.advent.util

import scala.annotation.tailrec

object MyMath {
  def lcm(list: Seq[BigInt]):BigInt=list.foldLeft(1:BigInt){
    (a, b) => b * a /
      Stream.iterate((a,b)){case (x,y) => (y, x%y)}.dropWhile(_._2 != 0).head._1.abs
  }

  @tailrec
  def gcdByEuclidsAlgorithm(n1: Long, n2: Long): Long = {
    if (n2 == 0) return n1
    gcdByEuclidsAlgorithm(n2, n1 % n2)
  }
}
