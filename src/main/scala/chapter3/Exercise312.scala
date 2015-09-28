package chapter3

object Exercise312 extends App {
  @annotation.tailrec
  def foldLeft[A, B](as: List[A], z: B)(f: (A, B) => B): B = as match {
    case Nil     => z
    case x :: xs => foldLeft(xs, f(x, z))(f)
  }

  def reverse[A](as: List[A]): List[A] =
    foldLeft(as, List.empty[A])(::[A])

  println(reverse(List(1, 2, 3, 4, 5)))
}
