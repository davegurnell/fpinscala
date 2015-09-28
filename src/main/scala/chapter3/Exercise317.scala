package chapter3

object Exercise317 extends App {
  def stringify(in: List[Double]): List[String] = in match {
    case hd :: tl => s"[$hd]" :: stringify(tl)
    case Nil => Nil
  }

  println(stringify(List(1.0, 2.0, 3.0, 4.0, 5.0)))
}
