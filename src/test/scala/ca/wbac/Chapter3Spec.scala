package ca.wbac

import org.scalatest.FunSpec

class Chapter3Spec extends FunSpec {
  describe("Chapter 3") {
    it("Exercise 3.24: should return true for sub-lists within super lists") {
      def startsWith[A](sup: List[A], sub: List[A]): Boolean = (sup, sub) match {
        case (_, Nil) => true
        case (x :: xs, y :: ys) if x != y => false
        case (x :: xs, y :: ys) if x == y => startsWith(xs, ys)
      }

      def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = (sup, sub) match {
        case (_, Nil) => true
        case (xs, ys) if !xs.contains(ys.head) => false
        case (xs, ys) if !startsWith(xs, ys) => hasSubsequence(xs.tail, ys)
        case (xs, ys) if startsWith(xs, ys) => hasSubsequence(xs.tail, ys.tail)
      }

      assert(hasSubsequence(List(1, 2, 3, 4), List(2, 3, 4)))
    }

    it("Exercise 3.25: should count the nodes in a tree") {
      val ab = Branch(Leaf("a"), Leaf("b"))
      val cd = Branch(Leaf("c"), Leaf("d"))
      val tree = Branch(ab, cd)

      def size[A](tree: Tree[A]): Int = tree match {
        case Branch(left, right) => size(left) + size(right) + 1
        case Leaf(_) => 1
      }

      assert(size(tree) == 7)
    }
  }
}
