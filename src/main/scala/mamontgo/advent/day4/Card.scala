package mamontgo.advent.day4

case class Card(id: Int, winners: Seq[Int], selections: Seq[Int]) {
  def print = s"Card $id: ${winners.mkString(",")} | ${selections.mkString(",")}"
}

object Card {

  trait Cleanable[A] {
    def clean(a: A): A
  }

  implicit class Ext[A: Cleanable](a: A) {
    def clean: A = implicitly[Cleanable[A]].clean(a)
  }

  implicit val cleanString: Cleanable[String] = (data: String) => {
    data.trim.replace("  ", " ")
  }

  def fromString(data: String): Card = {

    val header = data.split(':')
    val numbers = header.last.split('|')
    Card(
      header.head.replace("Card ", "").clean.toInt,
      numbers.head.clean.split(' ').map(v => v.trim).map(t => t.toInt),
      numbers.last.clean.split(' ').map(v => v.trim).map(t => t.toInt)
    )
  }

  def calcPoints(c: Card): Int = {
    c.selections.foldLeft(0) { (i, v) =>
      c.winners.find(w => w == v).map(_ => if (i==0) 1 else i*2).getOrElse(i)
    }
  }

  def countMatches(c: Card): Int = {
    c.selections.foldLeft(0) { (i, v) =>
      c.winners.find(w => w == v).map(_ => i+1).getOrElse(i)
    }
  }

  def calcCards(cards: Seq[Card]): Int = {
    cards match {
      case h :: t => {
        val c = countMatches(h)
        if(c > 0) {
          (0 to c).foldLeft((1, t)) { (i, _) =>
            i match {
              case (count, t1) => {
                (count + calcCards(t1), t1)
              }
            }
          }
        }._1 else {
          1  + calcCards(t)
        }
      }
      case _ => 0
    }
  }


//  def extrapolateCards(cards: Seq[Card]): Seq[WinCard] = {
//    cards match {
//      case h :: t => {
//        val winCard = WinCard(h.id, h)
//        val c = countMatches(h)
//        val items = {
//          if (c > 0) {
//            (1 to c).foldLeft((Seq[Card](), t)) { (i, _) =>
//              i match {
//                case (l, t1) =>  (l :+ t1.head, t1.tail)
//              }
//            }
//          }._1 else {
//            Seq(h)
//          }
//        }
//        Seq(winCard) ++ extrapolateCards(items ++ t)
//      }
//      case _ => Seq()
//    }
//  }

  def countAllCards(l: Seq[Card]): Int = {
    countAllCards(0, l.head, l.tail)
  }

  def countAllCards(tally: Int, h: Card, l: Seq[Card]): Int = {
    val newTally = countCard(h, l) + tally
    l match {
      case lh :: ll =>  countAllCards(newTally, lh, ll)
      case _ => newTally
    }

  }

  def countCard(card: Card, l: Seq[Card]): Int = {
    val c = countMatches(card)
    if (c > 0) {
      (1 to c).foldLeft((1, l)) { (i, _) =>
        i match {
          case (l, t1) => (l + countCard(t1.head, t1.tail), t1.tail)
        }
      }._1
    } else {
      1
    }
  }
}