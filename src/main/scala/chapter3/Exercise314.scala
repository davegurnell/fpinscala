package chapter3

object Exercise314 extends App {
  @annotation.tailrec
  def foldLeft[A, B](as: List[A], z: B)(f: (A, B) => B): B = as match {
    case Nil     => z
    case x :: xs => foldLeft(xs, f(x, z))(f)
  }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B =
    foldLeft[A, (B) => B](as, b => b)((a, accum) => b => accum(f(a, b)))(z)

  def append[A](x: List[A], y: List[A]): List[A] =
    foldRight[A, List[A]](x, y)(_ :: _)

  println(append(List(1, 2, 3), List(4, 5, 6)))
}
