package chapter3

object Exercise328 extends App {
  sealed trait Tree[+A]
  case class Leaf[A](value: A) extends Tree[A]
  case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

  def map[A, B](tree: Tree[A])(fn: A => B): Tree[B] = tree match {
    case Leaf(n)      => Leaf(fn(n))
    case Branch(l, r) => Branch(map(l)(fn), map(r)(fn))
  }

  println(map(Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(3), Leaf(4))))(_ + 1))
}

