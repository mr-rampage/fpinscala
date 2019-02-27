package ca.wbac

import org.scalatest.FunSpec

import scala.collection.immutable.Stream.Empty

class Chapter4Spec extends FunSpec {

  describe("Exercise 4.1") {
    it("should map") {
      assert(Some(5).map(x => x * 2) == Some(10))
    }

    it("should return the value") {
      assert(Some(5).getOrElse(10) == 5)
      assert(None.getOrElse(10) == 10)
    }

    it("should return the option") {
      assert(Some(5).orElse(Some(10)) == Some(5))
      assert(None.orElse(Some(10)) == Some(10))
    }

    it("should filter the option") {
      assert(Some(5).filter(x => x % 2 == 0) == None)
      assert(Some(5).filter(x => x % 2 == 1) == Some(5))
    }

    it("should flatMap the option") {
      assert(Some(5).flatMap(_ => Some(10)) == Some(10))
      assert(Some(5).flatMap(_ => None) == None)
    }
  }

  describe("Exercise 4.2: Variance") {
    def variance(xs: Seq[Double]): Option[Double] =
      mean(xs) flatMap (avg => mean(xs map (x => math pow(x - avg, 2))))

    def mean(xs: Seq[Double]): Option[Double] = xs match {
      case Empty => None
      case _ => Some(xs.sum / xs.size)
    }

    it("should calculate the variance") {
      assert(variance(Seq()) == None)
      assert(variance(Seq(1, 2)) == Some(0.25))
    }
  }

  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    a flatMap (aa => b map (bb => f(aa, bb)))

  describe("Exercise 4.3: map2") {

    it("should return None if either a or b is None") {
      assert(map2(Some(20), None)((a, b) => a + b) == None)
      assert(map2(None, Some(2))((a, _) => a) == None)
    }

    it("should return the binary function") {
      assert(map2(Some(20), Some(10))(_ + _) == Some(30))
    }
  }

  describe("Exercise 4.4: sequence") {
    def sequence[A](as: List[Option[A]]): Option[List[A]] =
      as.foldLeft[Option[List[A]]](Some(List.empty))(map2(_, _)(_ :+ _))

    it("should return None if the input contains a single None") {
      assert(sequence(List(Some(4), Some(5), None)) == None)
    }

    it("should return a sequence") {
      assert(sequence(List(Some(4), Some(5))) == Some(List(4, 5)))
    }
  }

  describe("Exercise 4.5: traverse") {
    def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] =
      a.foldLeft[Option[List[B]]](Some(List.empty))((x, y) => map2(x, f(y))(_ :+ _))

    it("should return None if any items are None") {
      assert(traverse(List(4, 5, 6))(i => if (i == 6) None else Some(i)) == None)
    }

    it("should return optional list") {
      assert(traverse(List(4, 5))(Some(_)) == Some(List(4, 5)))
    }

    def sequence[A](as: List[Option[A]]): Option[List[A]] =
      traverse(as)(identity)

    it("should return None if the input contains a single None") {
      assert(sequence(List(Some(4), Some(5), None)) == None)
    }

    it("should return a sequence") {
      assert(sequence(List(Some(4), Some(5))) == Some(List(4, 5)))
    }
  }

  describe("Exercise 4.6: Either") {
    def test(value: Int): Either[String, Int] =
      if (value % 2 == 0)
        Right(value)
      else
        Left("Not divisible by two")

    it("should map correct values") {
      assert(test(4).map(_ * 2) == Right(8))
      assert(test(5).map(_ * 2) == Left("Not divisible by two"))
    }

    it("should flatmap correct values") {
      assert(Right(Right(5)).flatMap(identity) == Right(5))
      assert(Right(Left(5)).flatMap(identity) == Left(5))
    }

    it("should return a default value") {
      assert(Right(5).orElse(Right(6)) == Right(5))
      assert(Left(5).orElse(Right(6)) == Right(6))
    }

    it("should lift functions") {
      assert(test(4).map2(Right(6))(_ + _) == Right(10))
      assert(test(5).map2(Right(6))(_ + _) == Left("Not divisible by two"))
    }
  }

  describe("Exercise 4.7: Sequence & Traverse") {
    def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
      as.foldLeft[Either[E, List[B]]](Right(List.empty[B]))((x, y) => x.map2(f(y))(_ :+ _))

    def sequence[E, A](es: List[Either[E, A]]) : Either[E, List[A]] =
      traverse(es)(identity)

    it("should return a list") {
      assert(sequence(List(Right(5), Right(6), Right(7))) == Right(List(5, 6, 7)))
    }

    it("should return the first error") {
      assert(sequence(List(Right(5), Left("error"), Left("error2"), Right(6), Left("error6"))) == Left("error"))
    }


    def test(value: Int): Either[String, Int] =
      if (value % 2 == 0)
        Right(value)
      else
        Left("Not divisible by two")

    it("should traverse the list") {
      assert(traverse(List(2, 4, 6))(test) == Right(List(2, 4, 6)))
    }

    it("should return the first error in traverse") {
      assert(traverse(List(2, 3, 4, 6))(test) == Left("Not divisible by two"))
    }
  }
}

