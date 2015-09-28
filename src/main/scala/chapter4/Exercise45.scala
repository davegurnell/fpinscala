package chapter4

import scala.util.Try

object Exercise45 extends App {
  def sequence[A](list: List[Option[A]]): Option[List[A]] =
    list.foldRight(Option(List.empty[A])) { (item, accum) =>
      item.flatMap(item => accum.map(accum => item :: accum))
    }

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] =
    sequence(a map f)

  println(traverse(List("1", "2", "3"))(str => Try(str.toInt).toOption))
  println(traverse(List("1", "b", "3"))(str => Try(str.toInt).toOption))
  println(traverse(List[String]())(str => Try(str.toInt).toOption))
}
