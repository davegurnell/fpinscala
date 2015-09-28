package chapter2

object Exercise24 extends App {
  def uncurry[A, B, C](func: A => B => C): (A, B) => C =
    (a: A, b: B) => func(a)(b)

  val func = (a: Int) => (b: Int) => a + b

  println(uncurry(func)(1, 2))
}