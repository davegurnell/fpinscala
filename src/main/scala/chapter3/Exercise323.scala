package chapter3

object Exercise323 extends App {
  def zipWith[A, B, C](as: List[A], bs: List[B])(fn: (A, B) => C): List[C] = (as, bs) match {
    case (Nil, Nil) => Nil
    case (_  , Nil) => sys.error("Badness")
    case (Nil, _  ) => sys.error("Badness")
    case (ahd :: atl, bhd :: btl) => fn(ahd, bhd) :: zipWith(atl, btl)(fn)
  }

  println(zipWith(List(1, 2, 3), List(4, 5, 6))(_ + _))
  println(zipWith(List(1, 2, 3), List(4, 5, 6))(_.toString + _.toString))
}
