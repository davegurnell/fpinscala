package chapter3

object Exercise39 extends App {
  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = as match {
    case Nil => z
    case x :: xs => f(x, foldRight(xs, z)(f))
  }

  def length[A](as: List[A]): Int =
    foldRight(as, 0)((a, accum) => accum + 1)

  println(length(List(0, 1, 2, 3, 4, 5)))
}
