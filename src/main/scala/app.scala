package com.alexknvl.cse6331

import scala.language.implicitConversions
import scala.annotation.tailrec
import scala.collection. {mutable, immutable}
import spire.algebra.{Eq, Order}
import scala.reflect.ClassTag

object Sort {
  private def mergeSort[@specialized T]
    (array: Array[T], temp: Array[T], i: Int, j: Int)
    (implicit o: Order[T]): Int =
  {
    if (j - i <= 1) 0
    else {
      val m = i + (j - i) / 2

      mergeSort(array, temp, i, m) +
        mergeSort(array, temp, m, j) +
        merge(array, temp, i, m, j)
    }
  }
  private def merge[@specialized T]
    (array: Array[T], temp: Array[T], i: Int, m: Int, j: Int)
    (implicit o: Order[T]): Int =
  {
    var p = i
    var q = m
    var k = i
    var count = 0

    while (p < m && q < j) {
      o.compare(array(p), array(q)) match {
        case 1 =>
          temp(k) = array(q)
          q += 1
          k += 1
          count += m - p
        case 0 | -1 =>
          temp(k) = array(p)
          p += 1
          k += 1
      }
    }

    while (p < m) {
      temp(k) = array(p)
      p += 1
      k += 1
    }

    while (q < j) {
      temp(k) = array(q)
      q += 1
      k += 1
    }

    for (n <- i until j) array(n) = temp(n)

    count
  }

  def mergeSortInPlace[@specialized T]
    (array: Array[T], temp: Array[T] = null)
    (implicit o: Order[T]): Int =
  {
    if (array.size > 1) {
      val tmp = if (temp == null) array.clone() else temp
      require(tmp.size >= array.size)

      mergeSort(array, tmp, 0, array.size)
    } else 0
  }

  def heapSortInPlace[@specialized T]
    (array: Array[T])
    (implicit o: Order[T]): Unit =
  {
    HeapOps.heapify(array, array.size)

    for (i <- array.size - 1 to 0 by -1) {
      HeapOps.swap(array, 0, i)
      HeapOps.shakeDown(array, i, 0)
    }
  }

  @inline def swap[@specialized T](array: Array[T], i: Int, j: Int): Unit = {
    val t = array(i)
    array(i) = array(j)
    array(j) = t
  }
  @inline def swap3[@specialized T](array: Array[T], i: Int, j: Int, k: Int): Unit = {
    val t = array(i)
    array(i) = array(j)
    array(j) = array(k)
    array(k) = t
  }

  def partition[@specialized T](array: Array[T], pivot: T, p: Int, r: Int)(implicit o: Order[T]): (Int, Int) = {
    require(0 <= p && p <= r && r <= array.size)

    var i = p
    var j = p
    for (k <- p until r) {
      o.compare(array(k), pivot) match {
        case 1 => ()
        case 0 =>
          swap(array, k, j)
          j += 1
        case -1 =>
          swap3(array, k, j, i)
          i += 1
          j += 1
      }
    }

    (i, j)
  }

  def quickSort[@specialized T](array: Array[T], i: Int, j: Int)(implicit o: Order[T]): Unit = {
    if (j - i > 1) {
      val (a, b) = partition(array, array(i), i, j)
      //println(s"$i $a $b $j ${array mkString ","}")
      quickSort(array, i, a)
      quickSort(array, b, j)
    }
  }

  def minMax[@specialized T](array: Array[T], key: T => Int, i: Int, j: Int): (Int, Int) = {
    var min = Int.MaxValue
    var max = Int.MinValue

    for (k <- i until j) {
      min = math.min(key(array(k)), min)
      max = math.max(key(array(k)), max)
    }

    (min, max)
  }

  def countingSort[@specialized T](array: Array[T], output: Array[T], key: T => Int, i: Int, j: Int): Unit = {
    if (array.size <= 1) return ()

    val (min, max) = minMax(array, key, i, j)
    val pos = Array.fill(max - min + 1)(0)
    def pkey(k: Int) = key(array(k)) - min

    //println(s"$min $max ${array mkString ","}")

    for (k <- i until j) {
      pos(pkey(k)) += 1
    }

    //println(s"${pos mkString ","}")

    for (k <- 1 until pos.size) {
      pos(k) += pos(k - 1)
    }

    //println(s"${pos mkString ","}")

    for (k <- j - 1 to 0 by -1) {
      val key = pkey(k)
      output(pos(key) - 1) = array(k)
      pos(key) -= 1
    }
  }

  def radixSort[@specialized T](array: Array[T], output: Array[T], key: T => Int, i: Int, j: Int): Unit = {
    if (array.size <= 1) return ()
  }

  @tailrec def orderStatistic[@specialized T](array: Array[T], i: Int, j: Int, n: Int)(implicit o: Order[T]): T = {
    //println(s"$i $j $n ${array mkString ","}")
    //println(s"${0 <= i && i <= j && j <= array.size} ${0 <= n && n < j - i}")
    require(0 <= i && i <= j && j <= array.size)
    require(0 <= n && n < j - i)

    if (j - i == 1) array(i)
    else {
      val (a, b) = partition(array, array(i + n), i, j)
      val (ra, rb) = (a - i, b - i)

      if (ra <= n && n < rb) array(a)
      else if (n < ra) orderStatistic(array, i, a, n)
      else if (n >= rb) orderStatistic(array, b, j, n - rb)
      else sys.error("Should never happen.")
    }
  }

  def lcs[@specialized T](a: Array[T], b: Array[T])(implicit eq: Eq[T]): List[(Int, Int)] = {
    def lcs0(i: Int, j: Int): (Int, List[(Int, Int)]) = {
      if (i == a.size || j == b.size) (0, Nil)
      else if (eq.eqv(a(i), b(j))) {
          val (c, l) = lcs0(i + 1, j + 1)
          (c + 1, (i, j) :: l)
      } else List(lcs0(i + 1, j), lcs0(i, j + 1)).maxBy(_._1)
    }

    lcs0(0, 0)._2
  }
}

object HeapOps {
  def parent(index: Int): Int = (index + 1) / 2 - 1
  def left(index: Int): Int = (index + 1) * 2 - 1
  def right(index: Int): Int = (index + 1) * 2

  def height(index: Int, size: Int): Int = {
    @tailrec def height0(index: Int, size: Int, acc: Int): Int = {
      if (index > size) acc
      else height0(left(index), size, acc + 1)
    }

    height0(index, size, 0)
  }

  def heapify[@specialized T](array: Array[T], size: Int)(implicit o: Order[T]): Unit = {
    for (i <- size/2 to 0 by -1) {
      HeapOps.shakeDown(array, size, i)
    }
  }

  @inline def swap[@specialized T](array: Array[T], i: Int, j: Int): Unit = {
    val t = array(i)
    array(i) = array(j)
    array(j) = t
  }

  def shakeUpOnce[@specialized T](array: Array[T], i: Int)(implicit o: Order[T]): Int = {
    if (i > 0) {
      val p: Int = parent(i)
      if (o.lt(array(p), array(i))) {
        swap(array, p, i)
        p
      } else i
    } else i
  }

  @tailrec def shakeUp[@specialized T](array: Array[T], i: Int)(implicit o: Order[T]): Unit = {
    val n = shakeUpOnce(array, i)
    if (n != i) shakeUp(array, n) else ()
  }

  def shakeDownOnce[@specialized T](array: Array[T], size: Int, i: Int)(implicit o: Order[T]): Int = {
    val l = left(i)
    val r = right(i)

    val n =
      if (r < size) {
        if (o.gt(array(l), array(r))) l else r
      }
      else if (l < size) l
      else i

    if (n != i) {
      if (o.lt(array(i), array(n))) {
        swap(array, i, n)
        n
      } else i
    } else i
  }

  @tailrec def shakeDown[@specialized T](array: Array[T], size: Int, i: Int)(implicit o: Order[T]): Unit = {
    val n = shakeDownOnce(array, size, i)
    if (n != i) shakeDown(array, size, n) else ()
  }
}

object SegmentProblem {
  def compress(segments: Seq[(Int, Int)]): (Array[Int], Array[Int], Array[Int]) = {
    val size = segments.size
    val (sorted, indices) = segments.zipWithIndex.sorted.toArray.unzip
    val (starts, ends) = sorted.unzip
    val rewards = sorted.map { case (s, f) => f - s }

    val next = Array.fill(size + 1)(size)
    next(size) = -1
    for (i <- 0 until size) {
      next(i) = BinarySearch.findLeft(starts, ends(i))
    }

    assert(rewards.size == size &&
           next.size    == size + 1 &&
           indices.size == size)

    assert(rewards.forall(_ > 0))
    assert(next.dropRight(1).forall(0 to size contains _))
    assert(next(size) == -1)
    assert(indices.forall(0 until size contains _))

    (rewards, next, indices)
  }

  def solve(segments: (Int, Int)*): Array[Boolean] = {
    require(segments.forall{case (s , f) => f - s > 0})

    val (rewards, next, indices) = compress(segments)
    val size = segments.size

    val memo = Array.fill(size + 1)(0)

    for (i <- size - 1 to 0 by -1) {
      memo(i) = math.max(memo(i + 1), memo(next(i)) + rewards(i))
    }

    val result = Array.fill(size)(false)

    var i = 0
    while (next(i) != -1) {
      if (memo(i) != memo(i + 1)) {
        result(indices(i)) = true
        i = next(i)
      } else {
        i += 1
      }
    }

    result
  }
}

object ShortestPath {
  // Edge typeclass.
  trait Edge[E] {
    def src(e: E): Int
    def dst(e: E): Int
  }

  case class Graph[N, E: Edge](
    nodes: IndexedSeq[N],
    adjacencyList: IndexedSeq[Seq[E]]) {
    require(nodes.size == adjacencyList.size)
  }

  case class WeightedEdge[T](src: Int, dst: Int, weight: T)
  implicit def weightedEdge[T] = new Edge[WeightedEdge[T]] {
    def src(e: WeightedEdge[T]) = e.src
    def dst(e: WeightedEdge[T]) = e.dst
  }

  object Graph {
    def apply(pairs: ((Int, Int), Double)*): Graph[Unit, WeightedEdge[Double]] = {
      val len = pairs.flatMap { case ((i1, i2), d) => List(i1, i2) }.max + 1

      def edgesFrom(i: Int) =
        pairs.filter { case ((i1, i2), d) => i1 == i }
             .map { case ((i1, i2), d) => WeightedEdge(i1, i2, d) }

      Graph(
        Array.fill[Unit](len)(()),
        Array.tabulate(len) (edgesFrom)
      )
    }
  }

  def shortest1[N](g: Graph[N, WeightedEdge[Double]], i: Int, j: Int): Double = {
    def shortest0(x: Int): Double = {
      if (x == j) 0
      else if (g.adjacencyList(x).size == 0) Double.PositiveInfinity
      else g.adjacencyList(x).map { e => e.weight + shortest0(e.dst) }.min
    }

    shortest0(i)
  }

  def shortest2[N](g: Graph[N, WeightedEdge[Double]], i: Int, j: Int): Double = {
    val memo = Array.fill[Double](g.nodes.size) { -1 }

    def shortest0(x: Int): Double = {
      if (memo(x) == -1) {
        if (x == j) {
          memo(x) = 0
        } else if (g.adjacencyList(x).size == 0) {
          memo(x) = Double.PositiveInfinity
        } else {
          memo(x) = g.adjacencyList(x)
            .map { e => e.weight + shortest0(e.dst) }
            .min
        }
      }

      memo(x)
    }

    shortest0(i)
  }
}

object LongestCommonSubsequence {
  // Recursive version, exponential time complexity.
  // Returns the length of LCS.
  def lcs1[T](a: IndexedSeq[T], b: IndexedSeq[T])(implicit eq: Eq[T]): Int = {
    def equal(i: Int, j: Int) = eq.eqv(a(i), b(j))

    def lcs0(i: Int, j: Int): Int = {
      if (i == a.size || j == b.size) 0
      else if (equal(i, j)) 1 + lcs0(i + 1, j + 1)
      else math.max(lcs0(i + 1, j), lcs0(i, j + 1))
    }

    lcs0(0, 0)
  }

  // Added memoization. Complexity reduced to O(a.size * b.size).
  def lcs2[T](a: IndexedSeq[T], b: IndexedSeq[T])(implicit eq: Eq[T]): Int = {
    def equal(i: Int, j: Int) = eq.eqv(a(i), b(j))

    val memo = Array.fill[Int](a.size, b.size) {-1}

    def lcs0(i: Int, j: Int): Int = {
      def lcs0_memo(i: Int, j: Int): Int = {
        if (i == a.size || j == b.size) 0
        else if (memo(i)(j) != -1) memo(i)(j)
        else lcs0(i, j)
      }

      if (i == a.size || j == b.size) 0
      else if (equal(i, j)) 1 + lcs0_memo(i + 1, j + 1)
      else math.max(lcs0_memo(i + 1, j), lcs0_memo(i, j + 1))
    }

    lcs0(0, 0)
  }

  // Recursion replaced with loops. Same time complexity.
  def lcs3[T](a: IndexedSeq[T], b: IndexedSeq[T])(implicit eq: Eq[T]): Int = {
    def equal(i: Int, j: Int) = eq.eqv(a(i), b(j))

    val memo = Array.tabulate[Int](a.size + 1, b.size + 1) {
      (i, j) =>
      if (i == a.size || j == b.size) 0
      else 0
    }

    for (i <- a.size - 1 to 0 by -1;
         j <- b.size - 1 to 0 by -1) {
      assert(memo(i + 1)(j + 1) != -1)
      assert(memo(i    )(j + 1) != -1)
      assert(memo(i + 1)(j    ) != -1)

      memo(i)(j) =
        if (i == a.size || j == b.size) 0
        else if (equal(i, j)) 1 + memo(i + 1)(j + 1)
        else math.max(memo(i + 1)(j), memo(i)(j + 1))
    }

    memo(0)(0)
  }

  // Recursive path (LCS) recovery.
  def lcs4[T](a: IndexedSeq[T], b: IndexedSeq[T])(implicit eq: Eq[T]): Seq[T] = {
    def equal(i: Int, j: Int) = eq.eqv(a(i), b(j))

    val memo = Array.tabulate[Int](a.size + 1, b.size + 1) {
      (i, j) =>
        if (i == a.size || j == b.size) 0
        else -1
    }

    for (i <- a.size - 1 to 0 by -1;
         j <- b.size - 1 to 0 by -1) {
      assert(memo(i + 1)(j + 1) != -1)
      assert(memo(i    )(j + 1) != -1)
      assert(memo(i + 1)(j    ) != -1)

      memo(i)(j) =
        if (i == a.size || j == b.size) 0
        else if (equal(i, j)) 1 + memo(i + 1)(j + 1)
        else math.max(memo(i + 1)(j), memo(i)(j + 1))
    }

    @tailrec def recoverPath(i: Int, j: Int, path: Seq[T]): Seq[T] = {
      if (i == a.size || j == b.size) path
      else if (equal(i, j)) recoverPath(i + 1, j + 1, a(i) +: path)
      else if (memo(i)(j) == memo(i + 1)(j)) recoverPath(i + 1, j, path)
      else {
        assert(memo(i)(j) == memo(i)(j + 1))
        recoverPath(i, j + 1, path)
      }
    }

    recoverPath(0, 0, Nil).reverse
  }

  // Loop-based path recovery.
  // Also added specialization for extra speed on primitive types.
  // Note that the return type is IndexedSeq[T].
  def lcs[@specialized T](a: IndexedSeq[T], b: IndexedSeq[T])
                         (implicit eq: Eq[T], ct: ClassTag[T]): IndexedSeq[T] = {
    def equal(i: Int, j: Int) = eq.eqv(a(i), b(j))

    val memo = Array.ofDim[Int](a.size + 1, b.size + 1)
    for (i <- 0 to a.size;
         j <- 0 to b.size) {
      memo(i)(j) =
        if (i == a.size || j == b.size) 0
        else -1
    }

    for (i <- a.size - 1 to 0 by -1;
         j <- b.size - 1 to 0 by -1) {
      assert(memo(i + 1)(j + 1) != -1)
      assert(memo(i    )(j + 1) != -1)
      assert(memo(i + 1)(j    ) != -1)

      memo(i)(j) =
        if (i == a.size || j == b.size) 0
        else if (equal(i, j)) 1 + memo(i + 1)(j + 1)
        else math.max(memo(i + 1)(j), memo(i)(j + 1))
    }

    var i: Int = 0
    var j: Int = 0
    var k: Int = 0
    val recovered = new Array[T](memo(0)(0))

    while (i != a.size && j != b.size) {
      if (equal(i, j)) {
        recovered(k) = a(i)
        i += 1
        j += 1
        k += 1
      } else if (memo(i)(j) == memo(i + 1)(j)) {
        i += 1
      } else {
        assert(memo(i)(j) == memo(i)(j + 1))
        j += 1
      }
    }

    recovered
  }
}

object BinarySearch {
  @tailrec def findLeft[T, V: Ordering](
    seq: IndexedSeq[T], key: T => V, i: Int, j: Int, value: V): Int =
  {
    require(i <= j)
    require(i <= seq.size)
    require(j <= seq.size)

    val o = implicitly[Ordering[V]]

    if (i == j) i
    else if (i + 1 == j) { if (o.lt(key(seq(i)), value)) j else i }
    else {
      val m = (i + j) / 2
      val middle = key(seq(m))

      if (o.gteq(middle, value)) findLeft(seq, key, i, m, value)
      else findLeft(seq, key, m + 1, j, value)
    }
  }

  @tailrec def findRight[T, V: Ordering](
    seq: IndexedSeq[T], key: T => V, i: Int, j: Int, value: V): Int =
  {
    require(i <= j)
    require(i <= seq.size)
    require(j <= seq.size)

    val o = implicitly[Ordering[V]]

    if (i == j) i
    else if (i + 1 == j) { if (o.gt(key(seq(i)), value)) i else j }
    else {
      val m = (i + j) / 2
      val middle = key(seq(m))

      if (o.lteq(middle, value)) findRight(seq, key, m, j, value)
      else findRight(seq, key, i, m, value)
    }
  }

  @tailrec def findLeft(seq: Array[Int], i: Int, j: Int, value: Int): Int = {
    require(i <= j)
    require(i <= seq.size)
    require(j <= seq.size)

    if (i == j) i
    else if (i + 1 == j) { if (seq(i) < value) j else i }
    else {
      val m = (i + j) / 2
      val middle = seq(m)

      if (middle >= value) findLeft(seq, i, m, value)
      else findLeft(seq, m + 1, j, value)
    }
  }

  @tailrec def findRight(seq: Array[Int], i: Int, j: Int, value: Int): Int = {
    require(i <= j)
    require(i <= seq.size)
    require(j <= seq.size)

    if (i == j) i
    else if (i + 1 == j) { if (seq(i) > value) i else j }
    else {
      val m = (i + j) / 2
      val middle = seq(m)

      if (middle <= value) findRight(seq, m, j, value)
      else findRight(seq, i, m, value)
    }
  }

  def findLeft(seq: Array[Int], value: Int): Int = findLeft(seq, 0, seq.size, value)
  def findRight(seq: Array[Int], value: Int): Int = findRight(seq, 0, seq.size, value)
}

object Sampler {
  private val rand = new java.util.Random()

  def iota(size: Int) : (Array[Int], Array[Int]) = {
    val set = mutable.Set.empty[Int]
    while (set.size < size) set += rand.nextInt(2 * size)

    val a = set.toArray.sorted
    val b = (0 until 2 * size).filter {!set.contains(_)}.toArray

    (a, b)
  }

  def rand(size: Int) : (Array[Int], Array[Int]) = {
    val set = mutable.Set.empty[Int]
    while (set.size < 2 * size) set += (rand.nextInt(size * 10) - size * 5)

    val v = set.toArray
    val a = v.slice(0, size).sorted
    val b = v.slice(size, 2 * size).sorted
    (a, b)
  }
}

object FindNth {
  @tailrec def find(seq1: Array[Int], i1: Int, j1: Int,
                    seq2: Array[Int], i2: Int, j2: Int,
                    n: Int): Int =
  {
    require(j1 - i1 == j2 - i2)
    require(j1 - i1 > 0)
    require(i1 <= seq1.size && j1 <= seq1.size)
    require(i2 <= seq2.size && j2 <= seq2.size)
    require(seq1.size == seq2.size)

    import BinarySearch.findLeft

    val m1 = (i1 + j1) / 2
    val m2 = (i2 + j2) / 2

    val v1 = seq1(m1)
    val v2 = seq2(m2)

    val r1 = m1 + findLeft(seq2, i2, j2, v1)
    val r2 = m2 + findLeft(seq1, i1, j1, v2)

    if (r1 == n) v1
    else if (r2 == n) v2
    else if (r1 > n && r2 > n) find(seq1, i1, m1, seq2, i2, m2, n)
    else if (r1 < n && r2 < n) find(seq1, m1 + 1, j1, seq2, m2 + 1, j2, n)
    else if (r1 > n && r2 < n) find(seq1, i1, m1, seq2, j2 - (m1 - i1), j2, n)
    else                       find(seq1, j1 - (m2 - i2), j1, seq2, i2, m2, n)
  }

  def find(seq1: Array[Int], seq2: Array[Int], n: Int): Int = {
    require(seq1.size == seq2.size)
    require(n < 2 * seq1.size)
    find(seq1, 0, seq1.size, seq2, 0, seq2.size, n)
  }

  @tailrec def findZero(a: Array[Int], b: Array[Int], i: Int, j: Int): Int = {
    require(a.size == b.size)
    val n = a.size
    def seq(i: Int) = a(i) - b(n - 1 - i)

    require(i <= j)
    require(i <= n)
    require(j <= n)

    if (i == j) i
    else if (i + 1 == j) { if (seq(i) < 0) j else i }
    else {
      val m = (i + j) / 2
      val middle = seq(m)

      if (middle >= 0) findZero(a, b, i, m)
      else findZero(a, b, m + 1, j)
    }
  }

  def findNth(a: Array[Int], b: Array[Int]): Int = {
    require(a.size == b.size)
    val n = a.size

    if (a(0) > b(n - 1)) return b(n - 1)
    if (b(0) > a(n - 1)) return a(n - 1)

    val i = findZero(a, b, 0, n)

    if (b(n - 1 - i) > a(i - 1)) b(n - 1 - i)
    else a(i - 1)
  }
}

object PointSampler {
  private val rand = new java.util.Random()

  def rand(size: Int) : (Array[Point]) = {
    var generated = 0
    val xSet = mutable.Set.empty[Double]
    val ySet = mutable.Set.empty[Double]
    val points = new Array[Point](size)
    while (generated < size) {
      val nx = rand.nextDouble()
      val ny = rand.nextDouble()

      if (!xSet.contains(nx) && !ySet.contains(ny)) {
        points(generated) = Point(nx, ny)
        generated += 1
      }
    }

    points
  }
}

sealed case class Point(x: Double, y: Double) {
  def +(that: Vector) = Point(this.x + that.x, this.y + that.y)
  def -(that: Vector) = Point(this.x - that.x, this.y - that.y)
  def -(that: Point) = Vector(this.x - that.x, this.y - that.y)
  def distanceTo(that: Point) = (this - that).length
}
sealed case class Vector(x: Double, y: Double) {
  def length = scala.math.sqrt(x * x + y * y)
}

class FiniteQueue[A](q: immutable.Queue[A]) {
  def enqueueFinite[B >: A](elem: B, maxSize: Int): immutable.Queue[B] = {
    var ret = q.enqueue(elem)
    while (ret.size > maxSize) { ret = ret.dequeue._2 }
    ret
  }
}
object FiniteQueue {
  implicit def toFiniteQueue[A](q: immutable.Queue[A]) = new FiniteQueue[A](q)
}

object ClosestPair {
  def merge[V: Ordering](values: IndexedSeq[V], l1: List[Int], l2: List[Int]): List[Int] = {
    val o = implicitly[Ordering[V]]

    @tailrec def merge0(l1: List[Int], l2: List[Int], l: List[Int]): List[Int] =
      (l1, l2) match {
        case (Nil, Nil) => l.reverse
        case (x :: xs, Nil) => merge0(xs, Nil, x :: l)
        case (Nil, y :: ys) => merge0(ys, Nil, y :: l)
        case (x :: xs, y :: ys) =>
          val kx = values(x)
          val ky = values(y)

          if (o.lt(kx, ky)) merge0(xs, l2, x :: l)
          else merge0(l1, ys, y :: l)
      }

    merge0(l1, l2, Nil)
  }

  def minDistance[T, @specialized V: Ordering](
    metric: (T, T) => V,
    from: T, rest: T*): (T, V) =
  {
    require(rest.nonEmpty)

    val o = implicitly[Ordering[V]]
    var minDistance = metric(from, rest.head)
    var minValue = rest.head

    for (value <- rest.tail) {
      val distance = metric(from, value)
      if (o.gt(minDistance, distance)) {
        minDistance = distance
        minValue = value
      }
    }

    (minValue, minDistance)
  }

  private[this] def fastSorted(points: IndexedSeq[Point]): (Int, Int) = {
    val ys = points.map { _.y }
    def metric(i: Int, j: Int): Double = {
      if (i == -1 || j == -1) Double.PositiveInfinity
      else points(i) distanceTo points(j)
    }

    def closestPairBetween(i: Int, m: Int, j: Int, d: Double, l: List[Int]): (Int, Int) = {
      require(0 <= i && i <= m && m <= j && j <= points.size)

      import FiniteQueue._
      import scala.math.abs

      var left = immutable.Queue.empty[Int]
      var right = immutable.Queue.empty[Int]

      var dr = d
      var (p, q) = (-1, -1)

      l.foreach { k =>
        val kx = points(k).x
        if (abs(kx - points(m).x) < d) {
          if (k <= m) {
            if (left.nonEmpty) {
              val (ki, kd) = minDistance(metric, k, left: _*)
              if (kd < dr) {
                dr = kd
                p = ki
                q = k
              }
            }
            right = right.enqueueFinite(k, 3)
          } else {
            if (right.nonEmpty) {
              val (ki, kd) = minDistance(metric, k, right: _*)
              if (kd < dr) {
                dr = kd
                p = ki
                q = k
              }
            }
            left = left.enqueueFinite(k, 3)
          }
        }
      }

      (p, q)
    }

    def closestPair(i: Int, j: Int): (Int, Int, List[Int]) = {
      require(0 <= i && i <= j && j < points.size)

      import scala.math.min

      if (j - i == 0) (-1, -1, List(i))
      else if (j - i == 1) {
        if (points(i).y < points(j).y) (i, j, List(i, j))
        else (j, i, List(j, i))
      } else {
        val m = (i + j) / 2

        val (p1, q1, l1) = closestPair(i, m)
        val (p2, q2, l2) = closestPair(m + 1, j)

        val l = merge(ys, l1, l2)

        val d1 = metric(p1, q1)
        val d2 = metric(p2, q2)
        val d = min(d1, d2)

        val (p3, q3) = closestPairBetween(i, m, j, d, l)
        val d3 = metric(p3, q3)

        if      (d1 <= d2 && d1 <= d3) (p1, q1, l)
        else if (d2 <= d1 && d2 <= d3) (p2, q2, l)
        else                           (p3, q3, l)
      }
    }

    val (p, q, _) = closestPair(0, points.size - 1)
    (p, q)
  }

  def fast(points: IndexedSeq[Point]): (Int, Int, Double) = {
    val (sorted, indices) =
      points.zipWithIndex.sortBy { case (Point(x, _), _) => x }.unzip

    val (i, j) = fastSorted(sorted)
    val (p, q) = (indices(i), indices(j))
    (scala.math.max(p, q), scala.math.min(p, q), points(p) distanceTo points(q))
  }

  private[this] def experimental0(points: IndexedSeq[Point]): (Int, Int) = {
    val ys = points.map { _.y }
    val sortedYs = ys.zipWithIndex.sortBy { case(y, _) => y }.unzip._2.toList

    def metric(i: Int, j: Int): Double = {
      if (i == -1 || j == -1) Double.PositiveInfinity
      else points(i) distanceTo points(j)
    }

    def closestPairBetween(i: Int, m: Int, j: Int, d: Double, l: List[Int]): (Int, Int) = {
      require(0 <= i && i <= m && m <= j && j <= points.size)

      import scala.math.abs

      val left = mutable.Queue.empty[Int]
      val right = mutable.Queue.empty[Int]

      var dr = d
      var (p, q) = (-1, -1)

      l.foreach { k =>
        val kx = points(k).x
        if (abs(kx - points(m).x) < d) {
          if (k <= m) {
            if (left.nonEmpty) {
              val (ki, kd) = minDistance(metric, k, left: _*)
              if (kd < dr) {
                dr = kd
                p = ki
                q = k
              }
            }
            right.enqueue(k)
            if (right.size > 3) right.dequeue()
          } else {
            if (right.nonEmpty) {
              val (ki, kd) = minDistance(metric, k, right: _*)
              if (kd < dr) {
                dr = kd
                p = ki
                q = k
              }
            }
            left.enqueue(k)
            if (left.size > 3) left.dequeue()
          }
        }
      }

      (p, q)
    }

    def closestPair(i: Int, j: Int): (Int, Int) = {
      require(0 <= i && i <= j && j < points.size)

      import scala.math.min

      if (j - i == 0) (-1, -1)
      else if (j - i == 1) {
        if (points(i).y < points(j).y) (i, j)
        else (j, i)
      } else {
        val m = (i + j) / 2

        val (p1, q1) = closestPair(i, m)
        val (p2, q2) = closestPair(m + 1, j)

        val d1 = metric(p1, q1)
        val d2 = metric(p2, q2)
        val d = min(d1, d2)

        val (p3, q3) = closestPairBetween(i, m, j, d, sortedYs)
        val d3 = metric(p3, q3)

        if      (d1 <= d2 && d1 <= d3) (p1, q1)
        else if (d2 <= d1 && d2 <= d3) (p2, q2)
        else                           (p3, q3)
      }
    }

    val (p, q) = closestPair(0, points.size - 1)
    (p, q)
  }

  def experimental(points: IndexedSeq[Point]): (Int, Int, Double) = {
    val (sorted, indices) =
      points.zipWithIndex.sortBy { case (Point(x, _), _) => x }.unzip

    val (i, j) = experimental0(sorted)
    val (p, q) = (indices(i), indices(j))
    (scala.math.max(p, q), scala.math.min(p, q), points(p) distanceTo points(q))
  }

  def naive(points: IndexedSeq[Point]): (Int, Int, Double) = {
    var minDist = Double.PositiveInfinity
    var minIndices = (-1, -1)

    for (i <- 0 until points.size;
         j <- 0 until i;
         d = points(i) distanceTo points(j)
    ) {
      if (minDist > d) {
        minDist = d
        minIndices = (i, j)
      }
    }

    (minIndices._1, minIndices._2, minDist)
  }
}

object PawnProblem {
  import shapeless.syntax.std.tuple._

  type Board = Array[Array[Int]]

  def correctBoard(board: Array[Array[Int]]): Boolean = {
    // Check for null values.
    if (board == null) return false
    if (!board.forall(a => a != null)) return false
    // Check for equal size sub-arrays.
    if (!(board, board drop 1).zipped.forall((a, b) => a.size == b.size)) return false

    // Get board size. Notice that we allow rectangular boards
    // as well as square boards.
    val rows = board.size
    val cols = board.headOption.map(_.size).getOrElse(0)

    // A reasonable requirement.
    if (rows < 1 || cols < 1) return false
    true
  }

  def checkBoard(board: Array[Array[Int]]): (Int, Int) = {
    // Check for null values.
    require(board != null)
    require(board.forall(a => a != null))
    // Check for equal size sub-arrays.
    require((board, board drop 1).zipped.forall((a, b) => a.size == b.size))

    // Get board size. Notice that we allow rectangular boards
    // as well as square boards.
    val rows = board.size
    val cols = board.headOption.map(_.size).getOrElse(0)

    // A reasonable requirement.
    require(rows >= 1 && cols >= 1)

    (rows, cols)
  }

  // An absolutely beautiful, fully functional solution.
  def solveRecursive(board: Array[Array[Int]], i: Int, j: Int): (Int, List[Int]) = {
    val (rows, cols) = checkBoard(board)
    require(0 until rows contains i)
    require(0 until cols contains j)

    if (i == rows - 1) (board(i)(j), Nil)
    else {
      val moves = List(-1, 0, 1)
        .filter(m => 0 <= j + m && j + m < cols)
        .map(m => solveRecursive(board, i + 1, j + m) :+ m)
      val (cost, path, move) = moves.minBy(_._1)
      (cost + board(i)(j), move :: path)
    }
  }

  // Recursive version with memoization.
  // Note that due to structural sharing, we effectively
  // (and efficiently) memoize a graph of possible moves.
  def solveMemoizedRecursive(board: Array[Array[Int]], i: Int, j: Int): (Int, List[Int]) = {
    val (rows, cols) = checkBoard(board)
    require(0 until rows contains i)
    require(0 until cols contains j)

    val memo = Array.fill[(Int, List[Int])](rows, cols)((-1, Nil))
    def proxy(i: Int, j: Int): (Int, List[Int]) =
      memo(i)(j) match {
        case (-1, _) =>
          val solution = solve0(i, j)
          memo(i)(j) = solution
          solution
        case solution => solution
      }
    for (j <- 0 until cols) memo(rows - 1)(j) = solve0(rows - 1, j)

    def solve0(i: Int, j: Int): (Int, List[Int]) = {
      if (i == rows - 1) (board(i)(j), Nil)
      else {
        val moves = List(-1, 0, 1)
          .filter(m => 0 <= j + m && j + m < cols)
          .map(m => proxy(i + 1, j + m) :+ m)
        val (cost, path, move) = moves.minBy(_._1)
        (cost + board(i)(j), move :: path)
      }
    }

    solve0(i, j)
  }

  // Iterative version.
  def solveIterative(board: Array[Array[Int]], i: Int, j: Int): (Int, List[Int]) = {
    val (rows, cols) = checkBoard(board)
    require(0 until rows contains i)
    require(0 until cols contains j)

    if (i == rows - 1) return (board(i)(j), Nil)

    val memo = Array.fill[(Int, List[Int])](rows, cols)((-1, Nil))
    for (c <- 0 until cols; r = rows - 1)
      memo(r)(c) = (board(r)(c), Nil)

    for (r <- rows - 2 to i by -1; c <- 0 until cols) {
      val moves = List(-1, 0, 1)
        .filter(m => 0 <= c + m && c + m < cols)
        .map(m => memo(r + 1)(c + m) :+ m)
      val (cost, path, move) = moves.minBy(_._1)
      memo(r)(c) = (cost + board(r)(c), move :: path)
    }

    memo(i)(j)
  }

  // Faster iterative version (unfortunately I can not see
  // any way to reduce memory complexity, since I need to
  // store paths).
  def solveIterative1D(board: Array[Array[Int]], i: Int, j: Int): (Int, List[Int]) = {
    val (rows, cols) = checkBoard(board)
    require(0 until rows contains i)
    require(0 until cols contains j)

    if (i == rows - 1) return (board(i)(j), Nil)

    var prevMemo = Array.fill[(Int, List[Int])](cols)((-1, Nil))
    var memo  = Array.fill[(Int, List[Int])](cols)((-1, Nil))

    for (c <- 0 until cols; r = rows - 1)
      prevMemo(c) = (board(r)(c), Nil)

    for (r <- rows - 2 to i by -1) {
      for (c <- 0 until cols) {
        val moves = List(-1, 0, 1)
          .filter(m => 0 <= c + m && c + m < cols)
          .map(m => prevMemo(c + m) :+ m)
        val (cost, path, move) = moves.minBy(_._1)
        memo(c) = (cost + board(r)(c), move :: path)
      }
      // Swap arrays.
      val t = prevMemo
      prevMemo = memo
      memo = t
    }

    prevMemo(j)
  }
}

object EditDistance {
  def min(i: Int*): Int = i.foldLeft(Int.MaxValue)(math.min)
  def max(i: Int*): Int = i.foldLeft(Int.MinValue)(math.max)

  def findRecursive(a: Seq[Int], b: Seq[Int]): Int = {
    val maxSize = math.max(a.size, b.size)
    val minSize = math.min(a.size, b.size)

    if (maxSize == 0) 0
    else if (minSize == 0) maxSize
    else {
      min(
        if (a(0) == b(0)) findRecursive(a.slice(1, a.size), b.slice(1, b.size))
        else findRecursive(a.slice(1, a.size), b.slice(1, b.size)) + 1,
        findRecursive(a.slice(1, a.size), b) + 1,
        findRecursive(a, b.slice(1, b.size)) + 1
      )
    }
  }

  def findRecursiveIndices(a: IndexedSeq[Int], b: IndexedSeq[Int]): Int = {
    def find0(i: Int, j: Int): Int = {
      require(i <= a.size)
      require(j <= b.size)
      val maxSize = math.max(a.size - i, b.size - j)
      val minSize = math.min(a.size - i, b.size - j)

      if (maxSize == 0) 0
      else if (minSize == 0) maxSize
      else {
        min(
          find0(i + 1, j + 1) + (if (a(i) == b(j)) 0 else 1),
          find0(i + 1, j) + 1,
          find0(i, j + 1) + 1
        )
      }
    }

    find0(0, 0)
  }

  def findMemoized(a: IndexedSeq[Int], b: IndexedSeq[Int]): Int = {
    val memo = Array.fill(a.size + 1, b.size + 1)(-1)
    def proxy(i: Int, j: Int): Int = {
      if (memo(i)(j) == -1) {
        val r = find0(i, j)
        memo(i)(j) = r
        r
      } else memo(i)(j)
    }

    def find0(i: Int, j: Int): Int = {
      require(i <= a.size)
      require(j <= b.size)
      val maxSize = math.max(a.size - i, b.size - j)
      val minSize = math.min(a.size - i, b.size - j)

      if (maxSize == 0) 0
      else if (minSize == 0) maxSize
      else {
        min(
          proxy(i + 1, j + 1) + (if (a(i) == b(j)) 0 else 1),
          proxy(i + 1, j) + 1,
          proxy(i, j + 1) + 1
        )
      }
    }

    proxy(0, 0)
  }

  def findIterative2D(a: IndexedSeq[Int], b: IndexedSeq[Int]): Int = {
    val memo = Array.fill(a.size + 1, b.size + 1)(-1)

    for (i <- a.size to 0 by -1; j <- b.size to 0 by -1) {
      val maxSize = math.max(a.size - i, b.size - j)
      val minSize = math.min(a.size - i, b.size - j)

      memo(i)(j) =
        if (maxSize == 0) 0
        else if (minSize == 0) maxSize
        else {
          min(
            memo(i + 1)(j + 1) + (if (a(i) == b(j)) 0 else 1),
            memo(i + 1)(j) + 1,
            memo(i)(j + 1) + 1
          )
        }
    }

    memo(0)(0)
  }
  def findIterative1D(a: IndexedSeq[Int], b: IndexedSeq[Int]): Int = {
    if (a.size < b.size) return findIterative1D(b, a)

    val memo = Array.fill(2, b.size + 1)(-1)

    for (i <- a.size to 0 by -1) {
      for (j <- b.size to 0 by -1) {
        val maxSize = math.max(a.size - i, b.size - j)
        val minSize = math.min(a.size - i, b.size - j)

        memo(0)(j) =
          if (maxSize == 0) 0
          else if (minSize == 0) maxSize
          else {
            min(
              memo(1)(j + 1) + (if (a(i) == b(j)) 0 else 1),
              memo(1)(j) + 1,
              memo(0)(j + 1) + 1
            )
          }
      }

      val t = memo(0)
      memo(0) = memo(1)
      memo(1) = t
    }

    memo(1)(0)
  }
}

object Knapsack {
  // This augments tuples with "op" function, which allows us to modify tuple's
  // contents inline. (I wonder why there is no such function in the standard
  // library).
  implicit class Tuple2Op[T, U](t: (T, U)) {
    def op[V, W](f: T => V, g: U => W): (V, W) = (f(t._1), g(t._2))
    def op[V, W](f: (T => V, U => W)): (V, W) = op(f._1, f._2)
  }

  // Recursive solution. Relatively ugly.
  def solve(maxWeight: Int, items: IndexedSeq[(Int, Int)]): IndexedSeq[Boolean] = {
    require(maxWeight >= 0)
    require(items.forall { case (w, v) => w >= 0 && v >= 0 })

    val size = items.size

    def solve0(maxWeight: Int, index: Int): (Int, List[Boolean]) = {
      require(0 <= index && index <= size)
      require(maxWeight >= 0)

      if (index == size) (0, Nil)
      else {
        val (weight, value) = items(index)

        if (weight > maxWeight) solve0(maxWeight, index + 1).op(s => s, l => false :: l)
        else {
          List(
            solve0(maxWeight,          index + 1).op(s => s,         l => false :: l),
            solve0(maxWeight - weight, index + 1).op(s => s + value, l =>  true :: l)
          ).maxBy(t => t._1)
        }
      }
    }

    solve0(maxWeight, 0)._2.toArray
  }

  // WARNING: sub-optimal solution which is used for testing purposes only!
  def solveGreedy(maxWeight: Int, items: IndexedSeq[(Int, Int)]): IndexedSeq[Boolean] = {
    require(maxWeight >= 0)
    require(items.forall { case (w, v) => w >= 0 && v >= 0 })

    val size = items.size

    val indices = (0 until size).sortBy(i => items(i)._2.toDouble / items(i)._1)
    var totalWeight = 0
    val result = Array.fill(size)(false)

    for (i <- 0 until size) {
      val (weight, _) = items(indices(i))
      if (totalWeight + weight <= maxWeight) {
        result(indices(i)) = true
        totalWeight += weight
      }
    }

    result
  }

  // Iterative non-recursive solution.
  def solveIterative2D(maxWeight: Int, items: IndexedSeq[(Int, Int)]): IndexedSeq[Boolean] = {
    require(maxWeight >= 0)
    require(items.forall { case (w, v) => w >= 0 && v >= 0 })

    val size = items.size
    val memo = Array.fill(maxWeight + 1, size + 1)(-1, List.empty[Boolean])

    for (i <- size to 0 by -1; w <- 0 to maxWeight) {
      memo(w)(i) =
        if (i == size) (0, List.empty[Boolean])
        else {
          val (weight, value) = items(i)

          if (weight > w) memo(w)(i + 1).op(s => s, l => false :: l)
          else {
            List(
              memo(w)(i + 1).op(s => s,         l => false :: l),
              memo(w - weight)(i + 1).op(s => s + value, l =>  true :: l)
            ).maxBy(t => t._1)
          }
        }
    }

    memo(maxWeight)(0)._2.toArray
  }
}

object Kruskal {
  sealed class UnionFind(size: Int) {
    val values = Array.fill(size)(0)

    object Root {
      def unapply(x: Int): Option[Int] = {
        if (values(x) < 0) Some(-values(x))
        else None
      }
    }

    @tailrec final def find(x: Int): Int = x match {
      case Root(_) => x
      case p => find(p)
    }

    def union(a: Int, b: Int): Unit = (a, b) match {
      case (Root(depthA), Root(depthB)) =>
        if (depthA > depthB) values(b) = a
        else if (depthB < depthA) values(a) = b
        else {
          values(a) = b
          values(b) = -(depthB + 1)
        }
      case _ => union(find(a), find(b))
    }
  }
}
