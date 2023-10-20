
sealed trait FList[+A]
case object Nil extends FList[Nothing]
case class Cons[+A](head: A, tail: FList[A]) extends FList[A]

object FList {
   def sum(ints: FList[Int]): Int = ints match {
     case Nil => 0
     case Cons(x, xs) => x + sum(xs)
   }

   def product(ds: FList[Double]): Double = ds match {
     case Nil => 1.0
     case Cons(0.0, _) => 0.0
     case Cons(x, xs) => x * product(xs)
   }

   def tail[A](l: FList[A]): FList[A] = {
     l match {
       case Nil => sys.error("tail of empty list")
       case Cons(_, t) => t
     } 
   }

   def setHead[A](l: FList[A], h: A): FList[A] = {
     l match {
       case Nil => sys.error("setHead on empty list")
       case Cons(_, t) => Cons(h, t)
     }
   }

   def drop[A](l: FList[A], n: Int): FList[A] = {
     if (n <= 0) l 
     else {
       l match {
         case Nil => Nil
         case Cons(_, t) => drop(t, n - 1)
       }
     }
   }

   def dropWhile[A](l: FList[A])(f: A => Boolean): FList[A] = {
     l match {
       case Cons(h, t) if f(h) => dropWhile(t)(f)
       case _ => l
     }
   }
   
   def init[A](l: FList[A]): FList[A] = {
     l match {
       case Nil => sys.error("init of empty list")
       case Cons(_, Nil) => Nil
       case Cons(h, t) => Cons(h, init(t))
     }
   }

   def foldLeft[A, B](l: FList[A], z: B)(f: (B, A) => B): B = {
     l match {
       case Nil => z
       case Cons(h, t) => foldLeft(t, f(z, h))(f)
     }
   }

   def foldRight[A, B](l: FList[A], z: B)(f: (A, B) => B): B =
     foldLeft(reverse(l), z)((b, a) => f(a, b))
   
   def foldLeftViaFoldRight[A, B](l: FList[A], z: B)(f: (B, A) => B): B =
     foldRight(l, (b: B) => b)((a, g) => b => g(f(b, a)))(z)

   def reverse[A](l: FList[A]): FList[A] = foldLeft(l, FList[A]())((acc, h) => Cons(h, acc))

   def length[A](as: FList[A]): Int = ???
   
   def append[A](l: FList[A], r: FList[A]): FList[A] = 
     foldRight(l, r)(Cons(_, _))

   def concat[A](l: FList[FList[A]]): FList[A] = 
     foldRight(l, Nil: FList[A])(append)
   
   def map[A, B](l: FList[A])(f: A => B): FList[B] =
     foldRight(l, Nil: FList[B])((h, t) => Cons(f(h), t))
  
   def flatMap[A, B](l: FList[A])(f: A => FList[B]): FList[B] = 
     concat(map(l)(f))
   
   def zipWith[A, B, C](a: FList[A], b: FList[B])(f: (A, B) => C): FList[C] = (a, b) match {
      case (Nil, _) => Nil
      case (_, Nil) => Nil
      case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1, h2), zipWith(t1, t2)(f))
   }

   @annotation.tailrec
   def startsWith[A](l: FList[A], prefix: FList[A]): Boolean = (l, prefix) match {
     case (_, Nil) => true
     case (Cons(h, t), Cons(h2, t2)) if h == h2 => startsWith(t, t2)
     case _ => false
   }

   @annotation.tailrec
   def hasSubsequence[A](sup: FList[A], sub: FList[A]): Boolean = sup match {
     case Nil => sub == Nil
     case _ if startsWith(sup, sub) => true
     case Cons(h, t) => hasSubsequence(t, sub)
   }

   def apply[A](as: A*): FList[A] = 
     if (as.isEmpty) Nil
     else Cons(as.head, apply(as.tail: _*))
}  
