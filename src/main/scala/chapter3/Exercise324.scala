package chapter3

object Exercise324 extends App {
  def hasSubsequence[A](haystack: List[A], needle: List[A]): Boolean = (haystack, needle) match {
    case (Nil, _) => false
    case (_, Nil) => true
    case (hhd :: htl, nhd :: ntl) if hhd == nhd =>
      hasSubsequence(htl, ntl) || hasSubsequence(htl, needle)
    case (hhd :: htl, nhd :: ntl) =>
      hasSubsequence(htl, needle)
  }

  println(hasSubsequence(List(1, 2, 2, 1, 2, 3, 1, 2, 4), List(1, 2, 3)))
  println(hasSubsequence(List(1, 2, 2, 1, 2, 3, 1, 2, 4), List(1, 2, 5)))
}
