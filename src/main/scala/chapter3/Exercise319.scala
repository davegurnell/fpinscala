package chapter3

object Exercise319 extends App {
  def filter[A](as: List[A])(fn: A => Boolean): List[A] = as match {
    case hd :: tl if fn(hd) => hd :: filter(tl)(fn)
    case hd :: tl => filter(tl)(fn)
    case Nil => Nil
  }

  println(filter(List(1, 2, 3, 4, 5))(_ % 2 == 0))
}
