package chapter3

object Exercise37 extends App {
  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = as match {
    case Nil     => z
    case x :: xs => f(x, foldRight(xs, z)(f))
  }

  def product(nums: List[Int]) =
    foldRight(nums, 1)(_ * _)

  println(product(List(0, 1, 2, 3, 4, 5)))

  // Iteration can't be shortcut.
}
