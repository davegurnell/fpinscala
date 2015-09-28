package chapter2

object Exercise21 extends App {
  def fib(n: Int): Int = n match {
    case 0 => 0
    case 1 => 1
    case n => fib(n - 1) + fib(n - 2)
  }

  (1 to 20).foreach { n =>
    println(s"fib($n) = ${fib(n)}")
  }
}
