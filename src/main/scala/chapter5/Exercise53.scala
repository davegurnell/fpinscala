package chapter5

object Exercise53 extends App {
  sealed trait Stream[+A] {
    def toList: List[A] = this match {
      case Empty => Nil
      case Cons(h, t) => h() :: t().toList
    }

    def take(n: Int): Stream[A] = this match {
      case Empty => Empty
      case stream if n == 0 => Empty
      case Cons(hd, tl) => Cons(hd, () => tl().take(n - 1))
    }

    def takeWhile(fn: A => Boolean): Stream[A] = this match {
      case Empty => Empty
      case Cons(hd, tl) =>
        val head = hd()
        if(fn(head)) Cons(() => head, () => tl().takeWhile(fn)) else Empty
    }
  }

  case object Empty extends Stream[Nothing]
  case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

  object Stream {
    def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
      lazy val head = hd
      lazy val tail = tl
      Cons(() => head, () => tail)
    }

    def empty[A]: Stream[A] = Empty

    def apply[A](as: A*): Stream[A] =
      if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))

    def create[A](as: () => A*): Stream[A] =
      if (as.isEmpty) empty else Cons(as.head, () => create(as.tail: _*))
  }

  def lazify[A](value: A) = () => {
    println(s"Getting $value")
    value
  }

  def testStream = Stream.create((1 to 5).toList map (lazify) : _*)

  println(testStream.takeWhile(_ < 3).toList)
}
