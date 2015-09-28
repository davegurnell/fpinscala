package chapter3

object Exercise318 extends App {
  def map[A, B](as: List[A])(fn: A => B): List[B] = as match {
    case hd :: tl => fn(hd) :: map(tl)(fn)
    case Nil      => Nil
  }

  println(map(List(1, 2, 3, 4, 5))(_ + 1))
}
