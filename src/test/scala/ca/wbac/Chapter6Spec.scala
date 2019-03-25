package ca.wbac

import org.scalatest.FunSpec

class Chapter6Spec extends FunSpec {
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (i, r) = rng.nextInt
    (if (i < 0) -(i + 1) else i, r)
  }

  def double(rng: RNG): (Double, RNG) = {
    val (i, r) = nonNegativeInt(rng)
    (if (i == Int.MaxValue) i - 1 else i / Int.MaxValue.toDouble, r)
  }

  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i, r) = rng.nextInt
    val (d, r2) = double(r)
    ((i, d), r2)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val ((i, d), r) = intDouble(rng)
    ((d, i), r)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, r1) = double(rng)
    val (d2, r2) = double(r1)
    val (d3, r3) = double(r2)
    ((d1, d2, d3), r3)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    List.range[Int](0, count)
      .foldLeft((List.empty[Int], rng))((a: (List[Int], RNG), _) => {
        val (is: List[Int], r: RNG) = a
        val (i, r2) = r.nextInt
        (is :+ i, r2)
      })
  }

  type Rand[+A] = RNG => (A, RNG)
  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] = rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def niceDouble(rng: RNG): Rand[Double] = map(nonNegativeInt)(_ / (Int.MaxValue.toDouble + 1))

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = rng => {
    val (a, rng2) = ra(rng)
    val (b, rng3) = rb(rng2)
    (f(a, b), rng3)
  }

  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] = map2(ra, rb)((_, _))

  val randIntDouble: Rand[(Int, Double)] = both(int, double)
  val randDoubleInt: Rand[(Double, Int)] = both(double, int)

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs.foldLeft(unit(List.empty[A]))((b: Rand[List[A]], a: Rand[A]) => map2(b, a)(_ :+ _))

  def niceInts(count: Int)(rng: RNG): Rand[List[Int]] =
    sequence(List.fill(count)(int))

  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] = rng => {
    val (a, r) = f(rng)
    g(a)(r)
  }

  def niceMap[A, B](s: Rand[A])(f: A => B): Rand[B] = flatMap(s)(i => unit(f(i)))
  def niceMap2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = flatMap(ra)(a => niceMap(rb)(b => f(a, b)))

  def nonNegativeLessThan(n: Int): Rand[Int] = flatMap(nonNegativeInt) { i =>
    val mod = i % n
    if (i + (n - 1) - mod >= 0)
      unit(mod)
    else
      nonNegativeLessThan(n)
  }

  describe("Exercise 6.4") {
    it("should generate a list of random numbers") {
      assert(ints(3)(SimpleRNG(42))._1 == List(16159453, -1281479697, -340305902))
    }

    it("should generate a nonNegative Less than n") {
      assert(nonNegativeLessThan(1)(SimpleRNG(1))._1 == 0)
    }
  }

  describe("Exercise 6.11") {
    it("given a coin and a turn, when the machine is locked, it should dispense a candy") {
      val m = Machine(locked = true, 1, 0)
      assert(Candy.simulateMachine(List(Coin, Turn)).run(m)._1 == (0, 1))
    }

    it("given a turn, when the machine is locked, it should dispense a candy") {
      val m = Machine(locked = false, 1, 0)
      assert(Candy.simulateMachine(List(Coin, Turn)).run(m)._1 == (0, 0))
    }

    it("should do nothing, when the machine has no candies") {
      val m = Machine(locked = false, 0, 100)
      assert(Candy.simulateMachine(List(Turn, Coin, Turn)).run(m)._1 == (0, 100))
    }

    it("given a turn, when the machine is locked, then nothing should happen") {
      val m = Machine(locked = true, 1, 0)
      assert(Candy.simulateMachine(List(Turn)).run(m)._1 == (1, 0))
    }

    it("given a coin, when the machine is unlocked, then nothing should happen") {
      val m = Machine(locked = false, 1, 0)
      assert(Candy.simulateMachine(List(Coin, Coin, Coin)).run(m)._1 == (1, 0))
    }
  }
}
