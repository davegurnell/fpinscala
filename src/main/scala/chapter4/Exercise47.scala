package chapter4

object Exercise47 extends App {
  trait Either[+E, +A] {
    def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
      case Left(l) => Left(l)
      case Right(r) => f(r)
    }

    def orElse[EE >: E,B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
      case Left(l) => b
      case Right(r) => Right(r)
    }

    def map[B](f: A => B): Either[E, B] =
      this flatMap (r => Right(f(r)))

    def map2[EE >: E, B, C](that: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
      for {
        l <- this
        r <- that
      } yield f(l, r)
  }

  case class Left[+E](value: E) extends Either[E, Nothing]
  case class Right[+A](value: A) extends Either[Nothing, A]

  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] =
    es.foldRight[Either[E, List[A]]](Right(Nil)) { (item, accum) =>
      for {
        a <- item
        b <- accum
      } yield a :: b
    }

  def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
    sequence(as map f)

  println(sequence(List(Right(1), Right(2), Right(3))))
  println(sequence(List(Right(1), Left("Badness"), Right(3))))
  println(sequence(List()))

  def parse(str: String): Either[String, Int] =
    scala.util.Try(str.toInt) map (Right(_)) getOrElse (Left(s"Badness: $str"))

  println(traverse(List("1", "2", "3"))(parse))
  println(traverse(List("1", "b", "3"))(parse))
  println(traverse(List[String]())(parse))
}
