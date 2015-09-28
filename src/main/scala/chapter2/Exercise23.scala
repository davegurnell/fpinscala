package chapter2

object Exercise23 extends App {
  def curry[A, B, C](func: (A, B) => C): A => B => C =
    (a: A) => (b: B) => func(a, b)

  val func = (a: Int, b: Int) => a + b

  println(curry(func)(1)(2))
}