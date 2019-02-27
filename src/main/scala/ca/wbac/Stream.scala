package ca.wbac

import ca.wbac.Stream.{cons, empty}

sealed trait Stream[+A] {
  def toList: List[A] = this match {
    case Empty => List.empty
    case Cons(head, rest) => head() :: rest().toList
  }

  def take(n: Int): Stream[A] = this match {
    case Cons(head, rest) if n > 1 => cons(head(), rest().take(n - 1))
    case Cons(head, _) if n == 1 => cons(head(), empty)
    case _ => empty
  }

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

  def takeWhile(p: A => Boolean): Stream[A] = foldRight(empty[A])((a, b) => if (p(a)) cons(a, b) else empty)

  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case Cons(h, t) => f(h(), t().foldRight(z)(f))
    case _ => z
  }

  def exists(p: A => Boolean): Boolean = foldRight(false)((a, b) => p(a) || b)

  def forAll(p: A => Boolean): Boolean = foldRight(true)((a, b) => p(a) && b)

  def headOption: Option[A] = foldRight[Option[A]](None)((h, _) => Some(h))

  def map[B](p: A => B): Stream[B] = foldRight(empty[B])((a, b) => cons(p(a), b))

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

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))
}
