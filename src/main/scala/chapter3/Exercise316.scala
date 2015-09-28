package chapter3

object Exercise316 extends App {
  def add1(in: List[Int]): List[Int] = in match {
    case hd :: tl => (hd + 1) :: add1(tl)
    case Nil => Nil
  }

  println(add1(List(1, 2, 3, 4, 5)))
}
