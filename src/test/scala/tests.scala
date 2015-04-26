package com.alexknvl.cse6331

import org.scalacheck.Arbitrary
import org.scalatest._
import org.scalatest.junit._
import org.scalatest.prop._

class SegmentProblemTest extends FunSuite with Matchers with GeneratorDrivenPropertyChecks {
  import SegmentProblem._
  import org.scalacheck.Gen
  import org.scalacheck.Arbitrary

  test("Simple test.") {
    solve(1->2, 2->3, 1->4) should be(List(false, false, true))
    solve(1->2, 2->3, -1 -> 10) should be(List(false, false, true))
    solve(1->2, 2->3, 3->4, 3->5, 3->6, 3->7) should be(List(true, true, false, false, false, true))
  }

  test("Complicated test.") {
    solve(
      1->4,
      2->3,
      3->4,
      4->5,
      2->9,
      5->10,
      6->8
    ) should be (List(true, false, false, true, false, true, false))
  }
}

class SortTest extends FunSuite with Matchers with GeneratorDrivenPropertyChecks {
  import org.scalacheck.Gen
  import spire.implicits._

  test("mergeSort") {
    forAll { a: Array[Int] =>
      val arr = a.clone()
      Sort.mergeSortInPlace(arr)
      arr should be (arr.sorted)
    }

    Sort.mergeSortInPlace(Array(0, 1, 2)) should be (0)
    Sort.mergeSortInPlace(Array(0, 2, 1)) should be (1)
    Sort.mergeSortInPlace(Array(2, 0, 1)) should be (1 + 1)
    Sort.mergeSortInPlace(Array(2, 1, 0)) should be (2 + 1)
    Sort.mergeSortInPlace(Array(2, 3, 8, 6, 1)) should be (4 + 1)
  }

  test("heapSort") {
    forAll { a: Array[Int] =>
      val arr = a.clone()
      Sort.heapSortInPlace(arr)
      arr should be(arr.sorted)
    }
  }

  test("partition") {
    forAll { a: Array[Int] =>
      val array = a.clone()
      if (array.size > 0) {
        val pivot = array(0)
        val (q, t) = Sort.partition(array, pivot, 0, array.size)
        for (i <- 0 until q) array(i) should be < (pivot)
        for (i <- q until t) array(i) should be(pivot)
        for (i <- t + 1 until array.size) array(i) should be > (pivot)
      }
    }
  }

  test("quickSort") {
    forAll { a: Array[Int] =>
      val arr = a.clone()
      Sort.quickSort(arr, 0, arr.size)
      arr should be(arr.sorted)
    }
  }

  val smallNumbers = Gen.containerOf[Array, Int](Gen.choose(-10, 10))

  test("countingSort") {
    forAll(smallNumbers) { a =>
      val arr = a.clone
      val out = arr.clone
      Sort.countingSort(arr, out, {x: Int => x}, 0, arr.size)
      out should be(arr.sorted)
    }

    forAll(smallNumbers) { a =>
      val arr = a.clone
      val out = arr.clone
      Sort.countingSort(arr, out, {x: Int => x + 100}, 0, arr.size)
      out should be(arr.sorted)
    }
  }

  val arrayAndIndex = for {
    a <- Gen.nonEmptyListOf(Arbitrary.arbitrary[Int])
    b <- Gen.choose(0, a.size - 1)
  } yield (a, b)

  test("orderStatistic") {
    forAll(arrayAndIndex) { case (a: List[Int], n: Int) =>
      if (0 <= n && n < a.size) {
        val arr = a.toArray
        val sorted = arr.sorted
        Sort.orderStatistic(arr, 0, arr.size, n) should be(sorted(n))
      }
    }
  }
}

class KnapsackTest extends FunSuite with Matchers with GeneratorDrivenPropertyChecks {
  import org.scalacheck.Gen
  import Knapsack._
  import scala.collection.{mutable, immutable}

  def pairs[T, U](first: Gen[T], second: Gen[U]): Gen[(T, U)] = for {
    f <- first
    s <- second
  } yield (f, s)

  val problems = (for {
    maxWeight <- Gen.choose(1, 10)
    size <- Gen.choose(1, 10)
    items <- Gen.containerOfN[immutable.Vector, (Int, Int)](size, pairs(Gen.choose(1, 10), Gen.choose(1, 10)))
  } yield (maxWeight, items)).suchThat{case(w, i) => w >= 0}

  val solve: (Int, IndexedSeq[(Int, Int)]) => IndexedSeq[Boolean] = solveIterative2D

  def checkSolution(maxWeight: Int, items: IndexedSeq[(Int, Int)],
                    solution: IndexedSeq[Boolean]): (Int, Int) = {
    solution.size should be (items.size)

    val weight = (0 until items.size).filter(solution).map(items(_)._1).sum
    val value  = (0 until items.size).filter(solution).map(items(_)._2).sum

    weight shouldBe <= (maxWeight)
    (weight, value)
  }

  test("Optimal doesn't lie.") {
    forAll(problems) { case (maxWeight, items) =>
      val solution = solve(maxWeight, items)
      checkSolution(maxWeight, items, solution)
    }
  }

  test("Optimal is better than greedy.") {
    forAll(problems) { case (maxWeight, items) =>
      val optimal = solve(maxWeight, items)
      val greedy = solveGreedy(maxWeight, items)
      val (_, optimalReward) = checkSolution(maxWeight, items, optimal)
      val (_, greedyReward) = checkSolution(maxWeight, items, greedy)

      optimalReward shouldBe >= (greedyReward)
    }
  }

  test("Tests.") {
    solve(26,
      Array(
        12 -> 24,
        7 -> 13,
        11 -> 23,
        8 -> 15,
        9 -> 16
      )
    ) should be(List(false, true, true, true, false))

    solve(165,
      Array(
        23 -> 92,
        31 -> 57,
        29 -> 49,
        44 -> 68,
        53 -> 60,
        38 -> 43,
        63 -> 67,
        85 -> 84,
        89 -> 87,
        82 -> 72
      )
      ) should be(List(true, true, true, true, false, true, false, false, false, false))

    solve(190,
      Array(
        56 -> 50,
        59 -> 50,
        80 -> 64,
        64 -> 46,
        75 -> 50,
        17 -> 5
      )
    ) should be(List(true, true, false, false, true, false))
  }
}

class EditDistanceTest extends FunSuite with Matchers with GeneratorDrivenPropertyChecks {
  import EditDistance._
  import org.scalacheck.Gen

  def ints(sizeGen: Gen[Int], valueGen: Gen[Int]): Gen[Array[Int]] =
    for {
      s <- sizeGen
      values <- Gen.containerOfN[Array, Int](s, valueGen)
    } yield values

  val find: (IndexedSeq[Int], IndexedSeq[Int]) => Int = findIterative1D
  val sequences = ints(Gen.choose(0, 6), Gen.choose(0, 10))

  test("Identity.") {
    forAll(sequences) { (a: Array[Int]) =>
      find(a, a) should be(0)
    }
  }

  test("Symmetry.") {
    forAll(sequences, sequences) { (a: Array[Int], b: Array[Int]) =>
      find(a, b) should be (find(b, a))
    }
  }

  test("Symmetry by reversal.") {
    forAll(sequences, sequences) { (a: Array[Int], b: Array[Int]) =>
      find(a, b) should be (find(a.reverse, b.reverse))
    }
  }

  test("Triangle inequality.") {
    forAll(sequences, sequences, sequences) { (a: Array[Int], b: Array[Int], c: Array[Int]) =>
      val x = find(b, c)
      val y = find(c, a)
      val z = find(a, b)
      x + y shouldBe >=(z)
      y + z shouldBe >=(x)
      z + x shouldBe >=(y)
    }
  }

  test("Consistency.") {
    forAll(sequences, sequences) { (a: Array[Int], b: Array[Int]) =>
      val solution = find(a, b)
      findRecursive(a, b) should be (solution)
      findRecursiveIndices(a, b) should be (solution)
      findMemoized(a, b) should be (solution)
      findIterative2D(a, b) should be (solution)
      findIterative1D(a, b) should be (solution)
    }
  }

  test("Equal strings.") {
    find(Array.empty[Int], Array.empty[Int]) should be (0)
    find(Array(1), Array(1)) should be (0)
    find(Array(1, 2), Array(1, 2)) should be (0)
    find(Array(1, 2, 3), Array(1, 2, 3)) should be (0)
    find(Array(1, 2, 3, 4), Array(1, 2, 3, 4)) should be (0)
  }

  test("One replacement.") {
    find(Array(1), Array(2)) should be (1)
    find(Array(1, 2), Array(1, 3)) should be (1)
    find(Array(1, 2), Array(3, 2)) should be (1)
    find(Array(1, 2, 3), Array(4, 2, 3)) should be (1)
    find(Array(1, 2, 3), Array(1, 4, 3)) should be (1)
    find(Array(1, 2, 3), Array(1, 2, 4)) should be (1)
    find(Array(5, 2, 3, 4), Array(1, 2, 3, 4)) should be (1)
    find(Array(1, 5, 3, 4), Array(1, 2, 3, 4)) should be (1)
    find(Array(1, 2, 5, 4), Array(1, 2, 3, 4)) should be (1)
    find(Array(1, 2, 3, 5), Array(1, 2, 3, 4)) should be (1)
  }

  test("Two replacements.") {
    find(Array(1, 2), Array(3, 4)) should be (2)
    find(Array(1, 2, 3), Array(4, 5, 3)) should be (2)
    find(Array(1, 2, 3), Array(1, 4, 5)) should be (2)
    find(Array(1, 2, 3), Array(4, 5, 3)) should be (2)
    find(Array(5, 6, 3, 4), Array(1, 2, 3, 4)) should be (2)
    find(Array(1, 5, 6, 4), Array(1, 2, 3, 4)) should be (2)
    find(Array(1, 2, 5, 6), Array(1, 2, 3, 4)) should be (2)
    find(Array(6, 2, 3, 5), Array(1, 2, 3, 4)) should be (2)
    find(Array(5, 2, 6, 4), Array(1, 2, 3, 4)) should be (2)
    find(Array(1, 5, 3, 6), Array(1, 2, 3, 4)) should be (2)
  }

  test("One addition.") {
    find(Array.empty[Int], Array(1)) should be(1)
    find(Array(1), Array(1, 2)) should be(1)
    find(Array(1), Array(2, 1)) should be(1)
    find(Array(1, 2), Array(1, 2, 3)) should be(1)
    find(Array(1, 2), Array(1, 3, 2)) should be(1)
    find(Array(1, 2), Array(3, 1, 2)) should be(1)
    find(Array(1, 2, 3), Array(1, 2, 3, 4)) should be(1)
    find(Array(1, 2, 3), Array(1, 2, 4, 3)) should be(1)
    find(Array(1, 2, 3), Array(1, 4, 2, 3)) should be(1)
    find(Array(1, 2, 3), Array(4, 1, 2, 3)) should be(1)
  }
}

class PawnProblemTest extends FunSuite with Matchers with GeneratorDrivenPropertyChecks {
  import PawnProblem._
  import org.scalacheck.Gen

  def board(widthGen: Gen[Int], heightGen: Gen[Int], valueGen: Gen[Int]): Gen[Board] =
    for {
      h <- heightGen
      w <- widthGen
      board <- Gen.containerOfN[Array, Array[Int]](
        h, Gen.containerOfN[Array, Int](w, valueGen)
      ).suchThat(correctBoard)
    } yield board

  val boards = board(Gen.choose(1, 10), Gen.choose(1, 10), Gen.choose(-100, 100))

  test("Consistency among different versions.") {
    forAll(boards) { board =>
      val solution = solveRecursive(board, 0, 0)

      solveMemoizedRecursive(board, 0, 0) should be(solution)
      solveIterative(board, 0, 0) should be(solution)
      solveIterative1D(board, 0, 0) should be(solution)
    }
  }
  test("One way.") {
    val board = Array(
      Array(0, 100, 100, 100),
      Array(100, 0, 100, 100),
      Array(100, 100, 0, 100),
      Array(100, 100, 0, 100)
    )

    solveIterative1D(board, 0, 0) should be (0, List(1, 1, 0))
  }
  test("Two ways, roadblock.") {
    val board = Array(
      Array(0, 100, 100, 100),
      Array(100, 0, 100, 100),
      Array(  0, 100, 0, 100),
      Array(  1, 1  , 1,   0)
    )

    solveIterative1D(board, 0, 0) should be (0, List(1, 1, 1))
  }
}

class ShortestPathTest extends FunSuite with Matchers {
  import ShortestPath._

  test("Simple graphs") {
    shortest1(Graph(
      0 -> 1 -> 1.0,
      0 -> 2 -> 3.0
    ), 0, 1) should be (1.0)

    shortest1(Graph(
      0 -> 1 -> 4.0,
      0 -> 2 -> 3.0,
      2 -> 1 -> 0.5
    ), 0, 1) should be (3.5)
  }
}

class LongestCommonSubsequenceTest extends FunSuite with Matchers {
  import LongestCommonSubsequence._
  import spire.implicits._

  test("Hand-written tests for lcs") {
    lcs(Array.empty[Int], Array.empty[Int]) should be (Nil)
    lcs(Array(1), Array.empty[Int]) should be (Nil)
    lcs(Array.empty[Int], Array(1)) should be (Nil)
    lcs(Array(1, 2), Array.empty[Int]) should be (Nil)
    lcs(Array.empty[Int], Array(1, 2)) should be (Nil)

    lcs(Array(1), Array(2)) should be (Nil)
    lcs(Array(1), Array(2, 3)) should be (Nil)
    lcs(Array(1, 2), Array(3, 4)) should be (Nil)

    lcs(Array(1, 2), Array(1, 1)) should be (1 +: Nil)
    lcs(Array(1, 2), Array(1, 1, 2, 2)) should be (1 +: 2 +: Nil)

    lcs(Array(1), Array(1)) should be (1 +: Nil)
    lcs(Array(1, 2), Array(1, 2)) should be (1 +: 2 +: Nil)
    lcs(Array(1, 2, 3), Array(1, 2, 3)) should be (1 +: 2 +: 3 +: Nil)

    lcs(Array(1, 2, 3), Array(2, 3)) should be (2 +: 3 +: Nil)
    lcs(Array(1, 2, 3), Array(3, 2, 3)) should be (2 +: 3 +: Nil)
    lcs(Array(1, 2, 3), Array(2)) should be (2 +: Nil)

    lcs(
      Array(-1, 1, -2, 2, -3, 3, 4),
      Array(-5, 1, -6, 2, -7, 3, -8)) should be (1 +: 2 +: 3 +: Nil)
  }
}

class ClosestPairTest extends FunSuite with Matchers {
  test("Random set of 1000 points 100 times") {
    for (i <- 0 until 100) {
      val p = PointSampler.rand(1000)

      val (i1, j1, d1) = ClosestPair.fast(p)
      val (i2, j2, d2) = ClosestPair.naive(p)

      d1 should be (d2)
    }
  }

  test("Random set of 2 points 10 times") {
    for (i <- 0 until 10) {
      val p = PointSampler.rand(2)

      val (i1, j1, d1) = ClosestPair.fast(p)
      val (i2, j2, d2) = ClosestPair.naive(p)

      d1 should be (d2)
    }
  }

  test("Random set of 4000 points 10 times") {
    for (i <- 0 until 10) {
      val p = PointSampler.rand(4000)

      val (i1, j1, d1) = ClosestPair.fast(p)
      val (i2, j2, d2) = ClosestPair.naive(p)

      d1 should be (d2)
    }
  }

  test("(exp) Random set of 1000 points 100 times") {
    for (i <- 0 until 100) {
      val p = PointSampler.rand(1000)

      val (i1, j1, d1) = ClosestPair.experimental(p)
      val (i2, j2, d2) = ClosestPair.naive(p)

      d1 should be (d2)
    }
  }

  test("(exp) Random set of 2 points 100 times") {
    for (i <- (0 until 100).par) {
      val p = PointSampler.rand(2)

      val (i1, j1, d1) = ClosestPair.experimental(p)
      val (i2, j2, d2) = ClosestPair.naive(p)

      d1 should be (d2)
    }
  }

  test("(exp) Random set of 3 points 100 times") {
    for (i <- 0 until 100) {
      val p = PointSampler.rand(3)

      val (i1, j1, d1) = ClosestPair.experimental(p)
      val (i2, j2, d2) = ClosestPair.naive(p)

      d1 should be (d2)
    }
  }

  test("(exp) Random set of 4 points 100 times") {
    for (i <- 0 until 100) {
      val p = PointSampler.rand(4)

      val (i1, j1, d1) = ClosestPair.experimental(p)
      val (i2, j2, d2) = ClosestPair.naive(p)

      d1 should be (d2)
    }
  }

  test("(exp) Random set of 4000 points 10 times") {
    for (i <- (0 until 10).par) {
      val p = PointSampler.rand(4000)

      val (i1, j1, d1) = ClosestPair.experimental(p)
      val (i2, j2, d2) = ClosestPair.naive(p)

      d1 should be (d2)
    }
  }

  test("hand-written") {
    val p = Array(Point(22, 11), Point(29, 12), Point(39, 8),  Point(40, 13), Point(31, 40), Point(3, 10))

    val (i1, j1, d1) = ClosestPair.experimental(p)
    val (i2, j2, d2) = ClosestPair.naive(p)
    d1 should be (d2)
  }
}

class BinarySearchTest extends FunSuite with Matchers {
  test("Testing findLeft: value is in the sequence.") {
    import BinarySearch._
    findLeft(Array(0, 1, 1, 2, 3), 0) should be(0)
    findLeft(Array(0, 1, 1, 2, 3), 1) should be(1)
    findLeft(Array(0, 1, 1, 2, 3), 2) should be(3)
    findLeft(Array(0, 1, 1, 2, 3, 3, 3, 4), 3) should be(4)
  }

  test("Testing findLeft: no such value.") {
    import BinarySearch._
    findLeft(Array(0, 1, 1, 2, 3), -1) should be(0)
    findLeft(Array(0, 1, 1, 2, 3), 4) should be(5)
    findLeft(Array(0, 2, 3), 1) should be(1)
  }

  test("Testing findRight: value is in the sequence.") {
    import BinarySearch._
    findRight(Array(0, 1, 1, 2, 3), 0) should be (1)
    findRight(Array(0, 1, 1, 2, 3), 1) should be (3)
    findRight(Array(0, 1, 1, 2, 3), 2) should be (4)
    findRight(Array(0, 1, 1, 2, 3, 3, 3, 4), 3) should be (7)
  }

  test("Testing findRight: no such value.") {
    import BinarySearch._
    findRight(Array(0, 1, 1, 2, 3), -1) should be (0)
    findRight(Array(0, 1, 1, 2, 3), 4) should be (5)
    findRight(Array(0, 2, 3), 1) should be (1)
  }
}

class FindFastNthSearchTest extends FunSuite with Matchers {
  test("findMiddlePoint on 4 elements thousand times") {
    val len = 2
    val iterations = 1000

    for (_ <- 0 until iterations) {

      val (a, b) = Sampler.iota(len)
      val i = FindNth.findZero(a, b, 0, len)
      if (a(len - 1) > b(0)) {
        (a(i) - b(len - 1 - i)) should be > 0
        if (i > 0) (a(i - 1) - b(len - i)) should be < 0
      } else {
        i should be (len)
      }
    }
  }

  test("findMiddlePoint on 20 elements one hundred thousand times") {
    val len = 10
    val iterations = 100000

    for (_ <- 0 until iterations) {

      val (a, b) = Sampler.iota(len)
      val i = FindNth.findZero(a, b, 0, len)
      if (a(len - 1) > b(0)) {
        (a(i) - b(len - 1 - i)) should be > 0
        if (i > 0) (a(i - 1) - b(len - i)) should be < 0
      } else {
        i should be (len)
      }
    }
  }

  test("findMiddlePoint on 200 elements thousand times") {
    val len = 100
    val iterations = 1000

    for (_ <- 0 until iterations) {

      val (a, b) = Sampler.iota(len)
      val i = FindNth.findZero(a, b, 0, len)
      if (a(len - 1) > b(0)) {
        (a(i) - b(len - 1 - i)) should be > 0
        if (i > 0) (a(i - 1) - b(len - i)) should be < 0
      } else {
        i should be (len)
      }
    }
  }

  test("2 elements ten times") {
    for (_ <- 0 until 100) {
      val (a, b) = Sampler.iota(1)
      FindNth.findNth(a, b) should be (0)
    }
  }

  test("20 elements ten thousand times") {
    for (_ <- 0 until 100000) {
      val (a, b) = Sampler.iota(10)
      FindNth.findNth(a, b) should be(9)
    }
  }

  test("2 random elements twenty times") {
    for (_ <- 0 until 200) {
      val (a, b) = Sampler.rand(1)
      val s = (a ++ b).sorted
      FindNth.findNth(a, b) should be(s(0))
    }
  }

  test("4 random elements twenty times") {
    for (_ <- 0 until 200) {
      val (a, b) = Sampler.rand(2)
      val s = (a ++ b).sorted
      FindNth.findNth(a, b) should be(s(1))
    }
  }

  test("20 random elements ten thousand times") {
    for (_ <- 0 until 100000) {
      val (a, b) = Sampler.rand(10)
      val s = (a ++ b).sorted
      FindNth.findNth(a, b) should be(s(9))
    }
  }

  test("3000 elements hundred times") {
    for (_ <- 0 until 1000) {
      val (a, b) = Sampler.iota(1500)
      FindNth.findNth(a, b) should be(1499)
    }
  }

  test("5000 elements hundred times") {
    for (_ <- 0 until 1000) {
      val (a, b) = Sampler.iota(2500)
      FindNth.findNth(a, b) should be(2499)
    }
  }
}

class FindNthSearchTest extends FunSuite with Matchers {
  test("2 elements ten times") {
    for (_ <- 0 until 10) {
      val (a, b) = Sampler.iota(1)
      for (n <- 0 until 2)
        FindNth.find(a, 0, a.size, b, 0, b.size, n) should be(n)
    }
  }

  test("20 elements ten thousand times") {
    for (_ <- 0 until 10000) {
      val (a, b) = Sampler.iota(10)
      for (n <- 0 until 20)
        FindNth.find(a, 0, a.size, b, 0, b.size, n) should be(n)
    }
  }

  test("20 random elements ten thousand times") {
    for (_ <- 0 until 10000) {
      val (a, b) = Sampler.rand(10)
      val s = (a ++ b).sorted
      for (n <- 0 until 20)
        FindNth.find(a, 0, a.size, b, 0, b.size, n) should be(s(n))
    }
  }

  test("3000 elements hundred times") {
    for (_ <- 0 until 100) {
      val (a, b) = Sampler.iota(1500)
      for (n <- 0 until 3000)
        FindNth.find(a, 0, a.size, b, 0, b.size, n) should be(n)
    }
  }

  test("5000 elements hundred times") {
    for (_ <- 0 until 100) {
      val (a, b) = Sampler.iota(2500)
      for (n <- 0 until 5000)
        FindNth.find(a, 0, a.size, b, 0, b.size, n) should be(n)
    }
  }

  test("Hand-written.") {
    FindNth.find(Array(-100, 0, 4), Array(1, 2, 9), 0) should be(-100)
    FindNth.find(Array(-100, 0, 4), Array(1, 2, 9), 1) should be(0)
    FindNth.find(Array(-100, 0, 4), Array(1, 2, 9), 2) should be(1)
    FindNth.find(Array(-100, 0, 4), Array(1, 2, 9), 3) should be(2)
    FindNth.find(Array(-100, 0, 4), Array(1, 2, 9), 4) should be(4)
    FindNth.find(Array(-100, 0, 4), Array(1, 2, 9), 5) should be(9)
  }
}

object FindNthBenchmark extends org.scalameter.PerformanceTest {
  import org.scalameter.api._

  val sizes: Gen[Int] = Gen.exponential("size")(1024, 20000000, 2)

  lazy val executor = LocalExecutor(
    new Executor.Warmer.Default,
    Aggregator.min,
    new Measurer.Default)
  lazy val persistor = Persistor.None
  lazy val reporter = ChartReporter(XYLine(scala.math.log, v => v))

  val arrays = for {
    size <- sizes
  } yield Sampler.rand(size / 2)

  performance of "FindNth" in {
    measure method "find" in {
      using (arrays) config (
        exec.benchRuns -> 5000
      ) in { case (a, b) =>
        FindNth.find(a, b, a.size)
      }
    }

    measure method "findFast" in {
      using (arrays) config (
        exec.benchRuns -> 10000
      ) in { case (a, b) =>
        FindNth.findNth(a, b)
      }
    }
  }
}

object ClosestPairBenchmark extends org.scalameter.PerformanceTest {
  import org.scalameter.api._

  val sizes: Gen[Int] = Gen.range("size")(2, 300, 1)

  lazy val executor = LocalExecutor(
    new Executor.Warmer.Default,
    Aggregator.min,
    new Measurer.Default)
  lazy val persistor = Persistor.None
  lazy val reporter = ChartReporter(XYLine(v =>v, v => v))

  val points = for {
    size <- sizes
  } yield PointSampler.rand(size)

  performance of "ClosestPair" in {
//    measure method "naive" in {
//      using (points)  in { ClosestPair.naive(_) }
//    }

    measure method "findFast" in {
      using (points) config (
        exec.benchRuns -> 1000
        ) in { ClosestPair.fast(_) }
    }

//    measure method "experimental" in {
//      using (points) config (
//        exec.benchRuns -> 100
//      ) in { ClosestPair.experimental(_) }
//    }
  }
}
