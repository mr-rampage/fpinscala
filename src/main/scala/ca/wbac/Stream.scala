package ca.wbac

import ca.wbac.Stream.{cons, empty, unfold}

sealed trait Stream[+A] {
  def toList: List[A] = this match {
    case Empty => List.empty
    case Cons(head, rest) => head() :: rest().toList
  }

  def take(n: Int): Stream[A] = unfold((this, n)) {
    case (Cons(head, rest), n) if n > 1 => Some(head(), (rest(), n - 1))
    case (Cons(head, rest), n) if n == 1 => Some(head(), (empty, 0))
    case _ => None
  }
  /*
  def take(n: Int): Stream[A] = this match {
    case Cons(head, rest) if n > 1 => cons(head(), rest().take(n - 1))
    case Cons(head, _) if n == 1 => cons(head(), empty)
    case _ => empty
  }
  */

  def drop(n: Int): Stream[A] = this match {
    case Cons(_, rest) if n > 0 => rest().drop(n - 1)
    case _ => this
  }

  /*
  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(head, rest) if p(head()) => Stream.cons(head(), rest().takeWhile(p))
    case _ => Stream.empty
  }
  */

  // def takeWhile(p: A => Boolean): Stream[A] = foldRight(empty[A])((a, b) => if (p(a)) cons(a, b) else empty)
  def takeWhile(p: A => Boolean): Stream[A] = unfold(this) {
    case Cons(head, rest) if p(head()) => Some(head(), rest())
    case _ => None
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case Cons(h, t) => f(h(), t().foldRight(z)(f))
    case _ => z
  }

  def exists(p: A => Boolean): Boolean = foldRight(false)((a, b) => p(a) || b)

  def forAll(p: A => Boolean): Boolean = foldRight(true)((a, b) => p(a) && b)

  def headOption: Option[A] = foldRight[Option[A]](None)((h, _) => Some(h))

  //  def map[B](p: A => B): Stream[B] = foldRight(empty[B])((a, b) => cons(p(a), b))

  def map[B](p: A => B): Stream[B] = unfold(this) {
    case Cons(h, t) => Some(p(h()), t())
    case _ => None
  }

  def zipWith[B, C](z: => Stream[B])(f: (A, B) => C): Stream[C] = unfold((this, z)) {
    case (Cons(h1, t1), Cons(h2, t2)) => Some(f(h1(), h2()), (t1(), t2()))
    case _ => None
  }

  def zipAll[B, C](z: => Stream[B])(f: (Option[A], Option[B]) => C): Stream[C] = unfold((this, z)) {
    case (Empty, Empty) => None
    case (Cons(h, t), Empty) => Some(f(Some(h()), None), (t(), empty))
    case (Empty, Cons(h, t)) => Some(f(None, Some(h())), (empty, t()))
    case (Cons(h1, t1), Cons(h2, t2)) => Some(f(Some(h1()), Some(h2())), (t1(), t2()))
  }

  def filter(p: A => Boolean): Stream[A] = foldRight(empty[A])((a, b) => if (p(a)) cons(a, b) else b)

  def append[B >: A](a: => Stream[B]): Stream[B] = foldRight(a)((a, b) => cons(a, b))

  def flatMap[B >: A](p: A => Stream[B]): Stream[B] = foldRight(empty[B])((a, b) => p(a).append(b))
}

case object Empty extends Stream[Nothing]

case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def constant[A](a: A): Stream[A] = unfold(a)(x => Some((x, x)))

  def from(n: Int): Stream[Int] = unfold(n)(x => Some((x, x+1)))

  //def fibs(p: Int = 0, n: Int = 1): Stream[Int] = cons(p, fibs(n, p + n))
  def fibs(): Stream[Int] = unfold((0, 1)) {
    case (p, n) => Some(p, (n, p + n))
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case Some((a, s)) => cons(a, unfold(s)(f))
    case None => empty
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))
}
