package chapter3

object Exercise329 extends App {
  sealed trait Tree[+A]
  case class Leaf[A](value: A) extends Tree[A]
  case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

  def fold[A, B](tree: Tree[A])(leafFn: A => B, branchFn: (B, B) => B): B = tree match {
    case Leaf(n)      => leafFn(n)
    case Branch(l, r) => branchFn(fold(l)(leafFn, branchFn), fold(r)(leafFn, branchFn))
  }

  def maximum(tree: Tree[Int]): Int =
    fold[Int, Int](tree)(a => a, _ max _)

  def maxDepth[A](tree: Tree[A]): Int =
    fold[A, Int](tree)(a => 1, (a: Int, b: Int) => 1 + (a max b))

  def map[A, B](tree: Tree[A])(fn: A => B): Tree[B] =
    fold[A, Tree[B]](tree)(a => Leaf(fn(a)), (a, b) => Branch(a, b))

  val data = Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(3), Leaf(4)))
  println(maximum(data))
  println(maxDepth(data))
  println(map(data)(_ + 1))
}
