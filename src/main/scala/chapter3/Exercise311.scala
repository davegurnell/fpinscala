package chapter3

object Exercise311 extends App {
  @annotation.tailrec
  def foldLeft[A, B](as: List[A], z: B)(f: (A, B) => B): B = as match {
    case Nil     => z
    case x :: xs => foldLeft(xs, f(x, z))(f)
  }

  def sum(as: List[Int]): Int =
    foldLeft(as, 0)(_ + _)

  def product(as: List[Int]): Int =
    foldLeft(as, 1)(_ * _)

  def length[A](as: List[A]): Int =
    foldLeft(as, 0)((a, accum) => accum + 1)

  println(sum(List(1, 2, 3, 4, 5)))
  println(product(List(1, 2, 3, 4, 5)))
  println(length(List(1, 2, 3, 4, 5)))
}
