package chapter4

object Exercise44 extends App {
  def sequence[A](list: List[Option[A]]): Option[List[A]] =
    list.foldRight(Option(List.empty[A])) { (item, accum) =>
      item.flatMap(item => accum.map(accum => item :: accum))
    }

  println(sequence(List(Some(1), Some(2), Some(3))))
  println(sequence(List(Some(1), None, Some(3))))
  println(sequence(List()))
}
