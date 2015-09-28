package chapter3

object Exercise34 extends App {
  def drop[A](as: List[A], n: Int): List[A] = as match {
    case as if n == 0 => as
    case hd :: tl => drop(tl, n - 1)
    case Nil     => sys.error("Badness")
  }

  println(drop(List(1, 2, 3, 4, 5), 3))
  println(drop(Nil, 3))
}
