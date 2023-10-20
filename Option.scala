sealed trait Option[+A]
case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Option {

  def map[A, B](value: Option[A])(f: A => B): Option[B] = value match {
    case None => None
    case Some(a) => Some(f(a))
  }

  def flatMap[A, B](value: Option[A], f: A => Option[B]): Option[B] = 
    getOrElse(map(value)(f), None)
  
  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = 
    flatMap[A, C](a, aa => map[B, C](b)(bb => f(aa, bb)))

  def getOrElse[A, B >: A](value: Option[B], default: => B): B = value match {
    case None => default
    case Some(a) => a
  }

  def orElse[A, B >: A](value: Option[A], default: => Option[B]): Option[B] =
    getOrElse(map(value)(Some(_)),default)

  def filter[A, B](value: Option[A])(f: A => Boolean): Option[A] = value match {
    case Some(a) if f(a) => Some(a)
    case _ => None
  }

  def sequence[A](a: List[Option[A]]): Option[List[A]] = {
   def combineOptions(
      current: List[Option[A]],
      result: Option[List[A]]): Option[List[A]] = current match {
    case Nil => map(result)(_.reverse)
    case Some(head) :: tail => combineOptions(tail, map(result)(list => head :: list))
    case None :: _ => None
   }

   combineOptions(a, Some(Nil))
  }

  
  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = a match {
    case Nil => Some(Nil)
    case h :: t => map2(f(h), traverse(t)(f))(_ :: _)
  }

}
