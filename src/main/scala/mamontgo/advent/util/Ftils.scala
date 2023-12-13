package mamontgo.advent.util

import scala.annotation.tailrec

object Ftils {
  @tailrec
  def foldUntil[T, K](x: Seq[T], init: K, test: K => Boolean, f: (K, T) => K): K = {
    x match {
      case h +: tail =>
        val y = f(init, h)
        if (test(y)) y
        else foldUntil(tail, y, test, f)
      case _ => init
    }
  }


  def mapWhile[T, K](x: Seq[T],  test: T => Boolean, f: T => K): (Seq[K], Seq[T]) = {
    mapWhile(x, (Seq(), Seq()), test, f)
  }

  @tailrec
  private def mapWhile[T, K](x: Seq[T], c:  (Seq[K], Seq[T]), test: T => Boolean, f: T => K): (Seq[K], Seq[T]) = {
    x match {
      case h :: t =>
        if(test(h)) {
          mapWhile(t,  (c._1 :+ f(h), c._2 ), test, f)
        } else {
          (c._1, t)
        }
      case _ => c
    }
  }
}
