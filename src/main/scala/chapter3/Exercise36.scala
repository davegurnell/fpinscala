package chapter3

object Exercise36 extends App {
  def init[A](as: List[A]): List[A] = as match {
    case Nil => sys.error("Badness")
    case hd :: Nil => Nil
    case hd :: tl => hd :: init(tl)
  }

  println(init[Int](List(1, 2, 3, 4, 5)))
  println(init[Int](Nil))
}
