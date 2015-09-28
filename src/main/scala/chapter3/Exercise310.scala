package chapter3

object Exercise310 extends App {
  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = as match {
    case Nil => z
    case x :: xs => f(x, foldRight(xs, z)(f))
  }

  @annotation.tailrec
  def foldLeft[A, B](as: List[A], z: B)(f: (A, B) => B): B = as match {
    case Nil     => z
    case x :: xs => foldLeft(xs, f(x, z))(f)
  }

  def lengthRight[A](as: List[A]): Int =
    foldRight(as, 0)((a, accum) => accum + 1)

  def lengthLeft[A](as: List[A]): Int =
    foldLeft(as, 0)((a, accum) => accum + 1)

  // println(lengthRight((1 to 10000).toList))

  println(lengthLeft((1 to 10000).toList))
}
