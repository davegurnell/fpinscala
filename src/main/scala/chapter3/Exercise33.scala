package chapter3

object Exercise33 extends App {
  def setHead[A](as: List[A], hd: A): List[A] = as match {
    case _ :: tl => hd :: tl
    case Nil     => sys.error("Badness")
  }

  println(setHead(List(1, 2, 3, 4, 5), 1000))
  println(setHead(Nil, 1000))
}