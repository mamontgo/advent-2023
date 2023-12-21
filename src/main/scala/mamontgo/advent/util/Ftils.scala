package mamontgo.advent.util

import scala.annotation.tailrec

object Ftils {

  def toMap[T, K, V](items: Seq[T], init: V, toKey: T => K, toValue: T => V, mergeValue: (V, V) => V): Map[K, V] = {
    items.foldLeft(Map[K, V]()) { (i, v) => {
        val key = toKey(v)
        i.removed(key) + (key -> mergeValue(i.getOrElse(key, init), toValue(v)))
      }
    }
  }

  @tailrec def foldUntil[T, K](x: Seq[T], init: K, test: K => Boolean)(f: (K, T) => K): K = {
    x match {
      case h +: tail => val y = f(init, h)
        if (test(y)) y else foldUntil(tail, y, test)(f)
      case _ => init
    }
  }


  @tailrec def foldUntilRes[T, K](x: Seq[T], init: K, test: T => Boolean)(f: (K, T) => K): K = {
    x match {
      case h +: tail => if (test(h)) init else foldUntilRes(tail, f(init, h), test)(f)
      case _ => init
    }
  }

  def mapWhile[T, K](x: Seq[T], test: T => Boolean, f: T => K): (Seq[K], Seq[T]) = {
    mapWhile(x, (Seq(), Seq()), test, f)
  }

  @tailrec private def mapWhile[T, K](x: Seq[T], c: (Seq[K], Seq[T]), test: T => Boolean, f: T => K): (Seq[K], Seq[T]) = {
    x match {
      case h :: t => if (test(h)) {
        mapWhile(t, (c._1 :+ f(h), c._2), test, f)
      } else {
        (c._1, t)
      }
      case _ => c
    }
  }


}
