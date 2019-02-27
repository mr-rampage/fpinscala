package ca.wbac

sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match {
    case Some(x) => Some(f(x))
    case None => None
  }

  def flatMap[B](f: A => Option[B]): Option[B] = this.map(f).getOrElse(None)

  def getOrElse[B >: A](default: => B): B = this match {
    case Some(x) => x
    case None => default
  }

  def orElse[B >: A](ob: => Option[B]): Option[B] = if (this == None) ob else this

  def filter(f: A => Boolean): Option[A] = this.map(f).flatMap(x => if (x) this else None)
}
case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]
