package chapter2

object Exercise22 extends App {
  def isSorted[A](arr: Array[A], ordered: (A, A) => Boolean): Boolean = {
    (0 to arr.length - 2).forall { n =>
      ordered(arr(n), arr(n + 1))
    }
  }

  println(isSorted[Int](Array(1, 2, 3, 4, 5), _ < _))
  println(isSorted[String](Array("a", "b", "c"), _ < _))
  println(isSorted[String](Array("a", "c", "b"), _ < _))
}
