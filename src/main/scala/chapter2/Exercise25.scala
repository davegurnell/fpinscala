package chapter2

object Exercise25 extends App {
  def compose[A, B, C](g: B => C, f: A => B): A => C =
    (a: A) => g(f(a))

  println(compose[Int, Double, String](_.toString + "!", _ / 2.0)(9))
}