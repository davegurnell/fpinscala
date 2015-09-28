package chapter3

object Exercise320 extends App {
  def flatMap[A, B](as: List[A])(fn: A => List[B]): List[B] = as match {
    case hd :: tl => fn(hd) ::: flatMap(tl)(fn)
    case Nil => Nil
  }

  println(flatMap(List(1, 2, 3))(i => List(i, i)))
}
