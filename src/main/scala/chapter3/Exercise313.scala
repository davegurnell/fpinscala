package chapter3

object Exercise313 extends App {
  @annotation.tailrec
  def foldLeft[A, B](as: List[A], z: B)(f: (A, B) => B): B = as match {
    case Nil     => z
    case x :: xs => foldLeft(xs, f(x, z))(f)
  }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B =
    foldLeft[A, (B) => B](as, b => b)((a, accum) => b => accum(f(a, b)))(z)

  def identity[A](as: List[A]): List[A] =
    foldRight(as, List.empty[A])(::[A])

  println(identity((1 to 10000).toList))
}
