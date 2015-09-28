package chapter3

object Exercise35 extends App {
  def dropWhile[A](as: List[A], fn: A => Boolean): List[A] = as match {
    case hd :: tl if fn(hd) => dropWhile(tl, fn)
    case as => as
  }

  println(dropWhile[Int](List(1, 2, 3, 4, 5), _ < 3))
  println(dropWhile[Int](Nil, _ < 3))
}
