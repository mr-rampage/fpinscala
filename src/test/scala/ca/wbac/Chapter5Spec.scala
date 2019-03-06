package ca.wbac

import ca.wbac.Stream._
import org.scalatest.FunSpec

class Chapter5Spec extends FunSpec {

  describe("Exercise 5.1: Stream to List") {
    it("should convert a stream to a list") {
      assert(Stream(5).toList == List(5))
    }
  }

  describe("Exercise 5.2: Take/Drop") {
    it("should take n items") {
      assert(Stream(5, 4, 3).take(2).toList == List(5, 4))
    }

    it("should drop n items") {
      assert(Stream(5, 4, 3).drop(2).toList == List(3))
    }
  }

  describe("Exercise 5.3: TakeWhile") {
    it("should take while a predicate is true") {
      assert(Stream(5, 4, 3).takeWhile(_ != 4).toList == List(5))
    }
  }

  describe("Exercise 5.4: TakeWhile") {
    it("should return true forAll when all values evaluate to true") {
      assert(Stream(5, 4, 3).forAll(_ > 0))
    }

    it("should return false forAll when all values evaluate to true") {
      assert(!Stream(5, -1, 3).forAll(_ > 0))
    }
  }

  describe("Exercise 5.6: headOption") {
    it("should return the head of a stream") {
      assert(Stream(5, 4).headOption == Some(5))
      assert(empty.headOption == None)
    }
  }

  describe("Exercise 5.7: map, filter, append, flatMap") {
    it("should map the values") {
      assert(Stream(5, 4, 3).map(_ * 2).toList == List(10, 8, 6))
    }

    it("should filter the values") {
      assert(Stream(5, 4, 3).filter(_ % 2 == 1).toList == List(5, 3))
    }

    it("should append a value") {
      assert(Stream(5, 4, 3).append(Stream(2)).toList == List(5, 4, 3, 2))
    }

    it("should flatmap a value") {
      assert(Stream(Stream(5, 4, 3), Stream(2, 1)).flatMap(identity).toList == List(5, 4, 3, 2, 1))
    }
  }


  describe("Exercise 5.8: constant") {
    it("should create an infinite stream") {
      assert(constant(5).take(5).toList == List(5, 5, 5, 5, 5))
    }
  }

  describe("Exercise 5.9: iterator") {
    it("should create a sequential list of numbers") {
      assert(from(5).take(3).toList == List(5, 6, 7))
    }
  }

  describe("Exercise 5.10: fibonacci") {
    it("should create the fibonacci sequence") {
      assert(fibs().take(7).toList == List(0, 1, 1, 2, 3, 5, 8))
    }
  }

  describe("Exercise 5.13: zipWith, zipAll") {
    it("should zip two streams") {
      assert(constant(5).zipWith(constant(1))((a, b) => a + b).take(5).toList == List(6, 6, 6, 6, 6))
    }

    it("should zip all streams if first stream is longer") {
      assert(constant(5).zipAll(constant(1).take(2))((a, b) => (a, b) match {
        case (Some(x), Some(y)) => x + y
        case (Some(x), None) => x
        case (None, Some(y)) => y
        case (None, None) => 0
      }).take(5).toList == List(6, 6, 5, 5, 5))
    }

    it("should zip all streams if second stream is longer") {
      assert(constant(5).take(2).zipAll(constant(1).take(10))((a, b) => (a, b) match {
        case (Some(x), Some(y)) => x + y
        case (Some(x), None) => x
        case (None, Some(y)) => y
        case (None, None) => 0
      }).take(5).toList == List(6, 6, 1, 1, 1))
    }
  }

  describe("Exercise 5.14: startswith") {
    it("should return true when the second stream is the prefix of the first") {
      assert(Stream(1, 2, 3).startsWith(Stream(1, 2)))
    }

    it("should return false when the second stream is not the prefix of the first") {
      assert(!Stream(1, 2, 3).startsWith(Stream(1, 2, 3, 4)))
    }
  }

  describe("Exercise 5.15: tails") {
    it("should return a Stream of suffix") {
      val source = Stream(1, 2)
      assert(source.tails.toList == List(source, source drop 1, Empty))
    }
  }

  describe("Exercise 5.16: scanRight") {
    it("should sum the suffix of a stream") {
      assert(Stream(1, 2, 3).scanRight(0)(_ + _).toList == List(6, 5, 3, 0))
    }
  }

}
