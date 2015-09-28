package chapter3

object Exercise315 extends App {
  @annotation.tailrec
  def foldLeft[A, B](as: List[A], z: B)(f: (A, B) => B): B = as match {
    case Nil     => z
    case x :: xs => foldLeft(xs, f(x, z))(f)
  }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B =
    foldLeft[A, (B) => B](as, b => b)((a, accum) => b => accum(f(a, b)))(z)

  def concatenate[A](lists: List[List[A]]): List[A] =
    foldRight[List[A], List[A]](lists, Nil) { (list, accum) =>
      foldRight[A, List[A]](list, accum)(_ :: _)
    }

  println(concatenate((1 to 100).map(n => n :: (1 to 99).map(_ => 0).toList).toList))
}
