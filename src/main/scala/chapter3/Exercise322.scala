package chapter3

object Exercise322 extends App {
  def sum(x: List[Int], y: List[Int]): List[Int] = (x, y) match {
    case (Nil, Nil) => Nil
    case (_  , Nil) => sys.error("Badness")
    case (Nil, _  ) => sys.error("Badness")
    case (xhd :: xtl, yhd :: ytl) => (xhd + yhd) :: sum(xtl, ytl)
  }

  println(sum(List(1, 2, 3), List(4, 5, 6)))
  println(sum(List(1, 2, 3), List(4, 5)))
}
