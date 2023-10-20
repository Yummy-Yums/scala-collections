sealed trait Stream[+A]
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl

    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def toList[A](s: Stream[A]): List[A] = s match {
    case Cons(h, t) => h() :: toList(t())
    case _ => List()
  }

  def take[A](s: Stream[A], n: Int): Stream[A] = s match {
    case Cons(h, t) if n > 0 => cons[A](h(), take(t(), n - 1))
    case Cons(h, _) if n == 0 => cons[A](h(), Stream.empty)
    case _ => Stream.empty
  }

  def drop[A](s: Stream[A], n: Int): Stream[A] = s match {
    case Cons(_, t) if n > 0 => drop(t(), n - 1)
    case _ => s
  }

  def takeWhile[A](s: Stream[A], f: A => Boolean): Stream[A] = s match {
    case Cons(h, t) if f(h()) => cons(h(), takeWhile(t(), f))
    case _ => Stream.empty
  }

  def exists[A](s: Stream[A], p: A => Boolean): Boolean = s match {
    case Cons(h, t) => p(h()) || exists(t(), p)
    case _ => false
  }

  def foldRight[A, B](s: Stream[A])(z: => B)(f: (A, => B) => B): B = s match {
    case Cons(h, t) => f(h(), foldRight(t())(z)(f))
    case _ => z
  }

  def forAll[A](s: Stream[A], f: A => Boolean): Boolean =
    foldRight(s)(true)((a, b) => f(a) && b)

  def takeWhile_1[A](s: Stream[A], f: A => Boolean): Stream[A] =
    foldRight(s)(empty[A])((h, t) =>
        if (f(h)) cons(h, t)
        else empty)
  
  def headOption[A](s: Stream[A]): Option[A] = foldRight(s)(None: Option[A])((h, _) => Some(h))

  def map[A, B](s: Stream[A], f: A => B): Stream[B] = foldRight(s)(empty[B])((h, t) => cons(f(h), t))

  def filter[A](s: Stream[A], f: A => Boolean): Stream[A] = 
    foldRight(s)(empty[A])((h, t) => if (f(h)) cons(h, t) else t)

  def append[B](s: Stream[B], s1: => Stream[B]): Stream[B] = foldRight(s)(s1)((h, t) => cons(h, t))

  def flatMap2[A, B](s: Stream[B], f: B => Stream[B]): Stream[B] = foldRight(s)(empty[B])((h, t) => append(f(h), t))

  def flatMap[A, B](s: Stream[B], f: B => Stream[A]): Stream[A] =
    foldRight(s)(empty[A])((h, t) => append(f(h), t))

  def apply[A](as: A*): Stream[A] = 
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))
}
