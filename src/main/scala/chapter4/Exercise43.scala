package chapter4

object Exercise43 extends App {
  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    a flatMap (a => b map (b => f(a, b)))

  println(map2[Int, Int, Int](Some(1), Some(2))(_ + _))
  println(map2[Int, Int, Int](None, Some(2))(_ + _))
  println(map2[Int, Int, Int](Some(1), None)(_ + _))
  println(map2[Int, Int, Int](None, None)(_ + _))
}
