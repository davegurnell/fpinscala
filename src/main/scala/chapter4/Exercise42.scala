package chapter4

object Exercise42 extends App {
  def mean(xs: Seq[Double]): Option[Double] =
    Option(xs.length).filter(_ > 0).map(xs.sum / _)

  def variance(xs: Seq[Double]): Option[Double] =
    mean(xs)
      .map(m => xs.map(x => math.pow(x - m, 2)))
      .flatMap(mean)

  println(mean(List(1, 2, 3, 4, 5)))
  println(variance(List(1, 2, 3, 4, 5)))

  println(mean(List(3, 3, 3, 3, 3)))
  println(variance(List(3, 3, 3, 3, 3)))

  println(mean(Nil))
  println(variance(Nil))
}
