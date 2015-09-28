package chapter3

object Exercise327 extends App {
  sealed trait Tree[+A]
  case class Leaf[A](value: A) extends Tree[A]
  case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

  def maxDepth(tree: Tree[Int]): Int = tree match {
    case Leaf(n)      => 1
    case Branch(l, r) => 1 + (maxDepth(l) max maxDepth(r))
  }

  println(maxDepth(Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(3), Leaf(4)))))
}

