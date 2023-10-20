sealed trait Either[+E, +A]
case class Left[+E](value: E) extends Either[E, Nothing]
case class Right[+A](value: A) extends Either[Nothing, A]

object Either {
  
  def map[A, E, B](value: Either[E, A], f: A => B): Either[E, B] = value match {
    case Right(a) => Right(f(a))
    case Left(e) => Left(e)
  }

  def orElse[E, A](value: Either[E, A], b: Either[E, A]): Either[E, A] = 
    value match {
      case Left(_) => b
      case Right(a) => Right(a)
    }

  def flatMap[A, B, E](value: Either[E, A], f: A => Either[E, B]): Either[E, B] = 
    value match {
      case Right(a) => f(a)
      case Left(e) => Left(e)
    }

  def map2[E, A, B, C](a: Either[E, A], b: Either[E, B])(f: (A, B) => C): Either[E, C] = {
    flatMap(a, (aa: A) => map(b, (bb: B) => f(aa, bb)))
  }

  def traverse[E, A, B](es: List[A])(f: A => Either[E, B]): Either[E, List[B]] = es match {
    case Nil => Right(Nil)
    case h :: t => map2(f(h), traverse(t)(f))(_ :: _)
  }

  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] = traverse(es)(x => x)
}

