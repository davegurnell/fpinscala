package chapter3

object Exercise38 extends App {
  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = as match {
    case Nil => z
    case x :: xs => f(x, foldRight(xs, z)(f))
  }

  def identity[A](as: List[A]): List[A] =
    foldRight(as, List.empty[A])(::.apply[A])

  println(identity(List(0, 1, 2, 3, 4, 5)))
}
