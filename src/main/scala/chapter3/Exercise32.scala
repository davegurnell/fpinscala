package chapter3

object Exercise32 extends App {
  def tail[A](as: List[A]): List[A] = as match {
    case hd :: tl => tl
    case Nil      => sys.error("Badness")
  }

  println(tail(List(1, 2, 3, 4, 5)))
  println(tail(Nil))
}