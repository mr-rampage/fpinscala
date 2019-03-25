package ca.wbac

case class State[S, +A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] = this.flatMap(a => State.unit(f(a)))
  def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] = this.flatMap(a => sb.map(b => f(a, b)))
  def flatMap[B](g: A => State[S, B]): State[S, B] = State { s =>
    val (a, s2) = run(s)
    g(a).run(s2)
  }
}

object State {
  def unit[S, A](a: A): State[S, A] = State{s:S => (a, s)}

  def sequence[S, A](fs: List[State[S, A]]): State[S, List[A]] =
    fs.foldLeft(State.unit[S, List[A]](List.empty[A]))((b, a) => b.map2(a)(_ :+ _))

  def modify[S](f: S => S): State[S, Unit] = for {
    s <- get
    _ <- set(f(s))
  } yield ()

  def get[S]: State[S, S] = State(s => (s, s))

  def set[S](s: S): State[S, Unit] = State(_ => ((), s))
}
