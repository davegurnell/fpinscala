package chapter4

object Exercise41 extends App {
  sealed trait Option[+A] {
    def map[B](f: A => B): Option[B] = this match {
      case Some(value) => Some(f(value))
      case None => None
    }

    def getOrElse[B >: A](default: => B): B = this match {
      case Some(value) => value
      case None => default
    }

    def flatMap[B](f: A => Option[B]): Option[B] =
      this map f getOrElse None

    def orElse[B >: A](ob: => Option[B]): Option[B] =
      this map (Some(_)) getOrElse ob

    def filter(f: A => Boolean): Option[A] = this flatMap {
      case value if f(value) => Some(value)
      case _ => None
    }
  }

  final case class Some[A](value: A) extends Option[A]
  final case object None extends Option[Nothing]

  println(Some(1) map (_ * 10))
  println(Some(0) map (_ * 10))
  println((None : Option[Int]) map (_.toString))

  println(Some(1) getOrElse 10)
  println(Some(0) getOrElse 10)
  println((None : Option[Int]) getOrElse 10)

  def testFunc(n: Int) = n match {
    case 1 => Some("It's one!")
    case _ => None
  }

  println(Some(1) flatMap testFunc)
  println(Some(0) flatMap testFunc)
  println((None : Option[Int]) flatMap testFunc)

  println(Some(1) map (_ == 1))
  println(Some(0) map (_ == 1))
  println((None : Option[Int]) filter (_ == 1))

}
