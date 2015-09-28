package chapter3

object Exercise321 extends App {
  def flatMap[A, B](as: List[A])(fn: A => List[B]): List[B] = as match {
    case hd :: tl => fn(hd) ::: flatMap(tl)(fn)
    case Nil => Nil
  }

  def filter[A](as: List[A])(fn: A => Boolean): List[A] = flatMap(as) {
    case a if fn(a) => List(a)
    case a => Nil
  }

  println(filter(List(1, 2, 3, 4, 5))(_ % 2 == 0))
}
