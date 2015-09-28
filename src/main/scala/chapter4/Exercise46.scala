package chapter4

object Exercise46 extends App {
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

  val good: Either[String, Int] = Right(123)
  val meh: Either[String, Int] = Right(0)
  val bad: Either[String, Int] = Left("Badness")

  println(good.flatMap(n => if(n == 0) Left("Zero") else Right(n)))
  println(meh.flatMap(n => if(n == 0) Left("Zero") else Right(n)))
  println(bad.flatMap(n => if(n == 0) Left("Zero") else Right(n)))

  println(good orElse Right(1000))
  println(meh orElse Right(1000))
  println(bad orElse Right(1000))

  println(good map (x => s"[$x]"))
  println(meh map (x => s"[$x]"))
  println(bad map (x => s"[$x]"))

  println(good.map2(meh)(_ + _))
  println(good.map2(bad)(_ + _))
  println(bad.map2(good)(_ + _))
}
