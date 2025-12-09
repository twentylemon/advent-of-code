package org.lemon.advent.lib

import org.lemon.advent.*
import org.scalacheck.{Arbitrary, Shrink, Gen}
import org.scalacheck.Prop.*
import org.scalacheck.Shrink.*
import org.scalacheck.Arbitrary.*

class RangeExtensionTest extends UnitTest:
  given Arbitrary[Range] = Arbitrary(for
    i <- Gen.choose(-10000, 10000)
    j <- Gen.choose(-10000, 10000)
    if i != j
  yield i to j by (j - i).sign)

  test("toNumericRange handles empty range") {
    (0 until 0).toNumericRange[Int].isEmpty shouldBe true
  }

  test("toNumericRange converts inclusive Range to NumericRange") {
    check((range: Range) => range.toSet == range.toNumericRange[Int].toSet)
  }

  test("toInterval converts Range to Interval") {
    check((range: Range) => range.min == range.toInterval.min && range.max == range.toInterval.max)
  }

class TupleExtensionTest extends UnitTest:
  test("toInterval creates empty interval when start > end") {
    check((a: Int, b: Int) => a > b ==> (a, b).toInterval.isEmpty)
  }

  test("toInterval creates interval from tuple") {
    check((a: Int, b: Int) => a <= b ==> { (a, b).toInterval.start == a && (a, b).toInterval.end == b })
  }

class EmptyIntervalTest extends UnitTest:
  test("is empty") {
    Interval.empty[Int].isEmpty shouldBe true
  }

  test("not nonEmpty") {
    Interval.empty[Int].nonEmpty shouldBe false
  }

  test("has size 0") {
    Interval.empty[Int].size shouldBe 0
  }

  test("has length 0") {
    Interval.empty[Int].length shouldBe 0
  }

  test("contains nothing") {
    check((n: Int) => !Interval.empty[Int].contains(n))
  }

  test("containsSlice empty interval") {
    Interval.empty[Int].containsSlice(Interval.empty) shouldBe true
  }

  test("does not containsSlice non-empty interval") {
    check((n: Int) => !Interval.empty[Int].containsSlice(Interval(n, n)))
  }

  test("head throws NoSuchElementException") {
    a[NoSuchElementException] should be thrownBy Interval.empty[Int].head
  }

  test("headOption is None") {
    Interval.empty[Int].headOption shouldBe None
  }

  test("last throws NoSuchElementException") {
    a[NoSuchElementException] should be thrownBy Interval.empty[Int].last
  }

  test("lastOption is None") {
    Interval.empty[Int].lastOption shouldBe None
  }

  test("min throws UnsupportedOperationException") {
    an[UnsupportedOperationException] should be thrownBy Interval.empty[Int].min
  }

  test("minOption is None") {
    Interval.empty[Int].minOption shouldBe None
  }

  test("max throws UnsupportedOperationException") {
    an[UnsupportedOperationException] should be thrownBy Interval.empty[Int].max
  }

  test("maxOption is None") {
    Interval.empty[Int].maxOption shouldBe None
  }

  test("iterator is empty") {
    Interval.empty[Int].iterator.isEmpty shouldBe true
  }

  test("indices is empty") {
    Interval.empty[Int].indices.isEmpty shouldBe true
  }

  test("tail throws UnsupportedOperationException") {
    an[UnsupportedOperationException] should be thrownBy Interval.empty[Int].tail
  }

  test("init throws UnsupportedOperationException") {
    an[UnsupportedOperationException] should be thrownBy Interval.empty[Int].init
  }

  test("slice returns empty") {
    check((from: Int, until: Int) => Interval.empty[Int].slice(from, until).isEmpty)
  }

  test("take returns empty") {
    check((n: Int) => Interval.empty[Int].take(n).isEmpty)
  }

  test("takeRight returns empty") {
    check((n: Int) => Interval.empty[Int].takeRight(n).isEmpty)
  }

  test("drop returns empty") {
    check((n: Int) => Interval.empty[Int].drop(n).isEmpty)
  }

  test("dropRight returns empty") {
    check((n: Int) => Interval.empty[Int].dropRight(n).isEmpty)
  }

  test("intersects nothing") {
    check((a: Int, b: Int) => !Interval.empty[Int].intersects(Interval(a, b)))
  }

  test("intersect returns empty") {
    check((a: Int, b: Int) => Interval.empty[Int].intersect(Interval(a, b)).isEmpty)
  }

  test("toRange is empty") {
    Interval.empty[Int].toRange.isEmpty shouldBe true
  }

  test("toNumericRange is empty") {
    Interval.empty[Int].toNumericRange.isEmpty shouldBe true
  }

  test("toString is Interval.empty") {
    Interval.empty[Int].toString shouldBe "Interval.empty"
  }

  test("isInclusive") {
    Interval.empty[Int].isInclusive shouldBe true
  }

class IntervalTest extends UnitTest:
  val intervalGen =
    for
      a <- Gen.choose(-1000, 1000)
      b <- Gen.choose(a, a + 1000)
    yield (a, b)

  def interval(a: Int, b: Int) = Interval(a, b)
  def range(a: Int, b: Int) = a to b
  given Arbitrary[Range] = Arbitrary(for
    i <- Gen.choose(-10000, 10000)
    j <- Gen.choose(-10000, 10000)
    if i != j
  yield i to j by (j - i).sign)

  test("Interval from Range preserves values") {
    check((range: Range) => Interval(range).toRange == (range.min to range.max))
  }

  test("Interval from empty Range is empty") {
    Interval(5 until 5).isEmpty shouldBe true
  }

  test("Interval from NumericRange preserves values") {
    check((range: Range) => Interval(range.toNumericRange[Int]).toRange == (range.min to range.max))
  }

  test("Interval from empty NumericRange is empty") {
    Interval((5 until 5).toNumericRange[Int]).isEmpty shouldBe true
  }

  test("Interval from descending Range normalizes to ascending") {
    val interval = Interval(10 to 5 by -1)
    interval.start shouldBe 5
    interval.end shouldBe 10
  }

  test("Interval rejects Range with step != 1 or -1") {
    an[IllegalArgumentException] should be thrownBy Interval(1 to 10 by 2)
  }

  test("isEmpty matches Range") {
    check(forAll(intervalGen) { (a, b) => interval(a, b).isEmpty == range(a, b).isEmpty })
  }

  test("nonEmpty matches Range") {
    check(forAll(intervalGen) { (a, b) => interval(a, b).nonEmpty == range(a, b).nonEmpty })
  }

  test("size matches Range") {
    check(forAll(intervalGen) { (a, b) => interval(a, b).size == range(a, b).size })
  }

  test("length matches Range") {
    check(forAll(intervalGen) { (a, b) => interval(a, b).length == range(a, b).length })
  }

  test("contains matches Range") {
    val gen =
      for
        (a, b) <- intervalGen
        x <- Gen.choose(a - 10, b + 10)
      yield (a, b, x)
    check(forAll(gen) { (a, b, x) => interval(a, b).contains(x) == range(a, b).contains(x) })
  }

  test("containsSlice matches Range") {
    val gen =
      for
        (a, b) <- intervalGen
        c <- Gen.choose(a - 10, b + 10)
        d <- Gen.choose(c, b + 10)
      yield (a, b, c, d)
    check(forAll(gen) { (a, b, c, d) =>
      interval(a, b).containsSlice(interval(c, d)) == range(a, b).containsSlice(range(c, d))
    })
  }

  test("apply matches Range") {
    val gen =
      for
        (a, b) <- intervalGen
        i <- Gen.choose(0, b - a)
      yield (a, b, i)
    check(forAll(gen) { (a, b, i) => interval(a, b)(i) == range(a, b)(i) })
  }

  test("head matches Range") {
    check(forAll(intervalGen) { (a, b) => interval(a, b).head == range(a, b).head })
  }

  test("headOption matches Range") {
    check(forAll(intervalGen) { (a, b) => interval(a, b).headOption == range(a, b).headOption })
  }

  test("last matches Range") {
    check(forAll(intervalGen) { (a, b) => interval(a, b).last == range(a, b).last })
  }

  test("lastOption matches Range") {
    check(forAll(intervalGen) { (a, b) => interval(a, b).lastOption == range(a, b).lastOption })
  }

  test("min matches Range") {
    check(forAll(intervalGen) { (a, b) => interval(a, b).min == range(a, b).min })
  }

  test("minOption matches Range") {
    check(forAll(intervalGen) { (a, b) => interval(a, b).minOption == range(a, b).minOption })
  }

  test("max matches Range") {
    check(forAll(intervalGen) { (a, b) => interval(a, b).max == range(a, b).max })
  }

  test("maxOption matches Range") {
    check(forAll(intervalGen) { (a, b) => interval(a, b).maxOption == range(a, b).maxOption })
  }

  test("iterator matches Range") {
    check(forAll(intervalGen) { (a, b) => interval(a, b).iterator.toSeq == range(a, b).iterator.toSeq })
  }

  test("indices matches Range") {
    check(forAll(intervalGen) { (a, b) => interval(a, b).indices.toSeq == range(a, b).indices.toSeq })
  }

  test("slice matches Range") {
    val gen =
      for
        (a, b) <- intervalGen
        from <- Gen.choose(-5, b - a + 5)
        until <- Gen.choose(from, b - a + 5)
      yield (a, b, from, until)
    check(forAll(gen) { (a, b, from, until) =>
      interval(a, b).slice(from, until).toSeq == range(a, b).slice(from, until).toSeq
    })
  }

  test("take matches Range") {
    val gen =
      for
        (a, b) <- intervalGen
        n <- Gen.choose(-5, b - a + 5)
      yield (a, b, n)
    check(forAll(gen) { (a, b, n) => interval(a, b).take(n).toSeq == range(a, b).take(n).toSeq })
  }

  test("takeRight matches Range") {
    val gen =
      for
        (a, b) <- intervalGen
        n <- Gen.choose(-5, b - a + 5)
      yield (a, b, n)
    check(forAll(gen) { (a, b, n) => interval(a, b).takeRight(n).toSeq == range(a, b).takeRight(n).toSeq })
  }

  test("drop matches Range") {
    val gen =
      for
        (a, b) <- intervalGen
        n <- Gen.choose(-5, b - a + 5)
      yield (a, b, n)
    check(forAll(gen) { (a, b, n) => interval(a, b).drop(n).toSeq == range(a, b).drop(n).toSeq })
  }

  test("dropRight matches Range") {
    val gen =
      for
        (a, b) <- intervalGen
        n <- Gen.choose(-5, b - a + 5)
      yield (a, b, n)
    check(forAll(gen) { (a, b, n) => interval(a, b).dropRight(n).toSeq == range(a, b).dropRight(n).toSeq })
  }

  test("splitAt matches Range") {
    val gen =
      for
        (a, b) <- intervalGen
        n <- Gen.choose(-5, b - a + 5)
      yield (a, b, n)
    check(forAll(gen) { (a, b, n) =>
      val (il, ir) = interval(a, b).splitAt(n)
      val (rl, rr) = range(a, b).splitAt(n)
      il.toSeq == rl.toSeq && ir.toSeq == rr.toSeq
    })
  }

  test("tail matches Range") {
    check(forAll(intervalGen) { (a, b) => interval(a, b).tail.toSeq == range(a, b).tail.toSeq })
  }

  test("init matches Range") {
    check(forAll(intervalGen) { (a, b) => interval(a, b).init.toSeq == range(a, b).init.toSeq })
  }

  test("indexOf matches Range") {
    val gen =
      for
        (a, b) <- intervalGen
        x <- Gen.choose(a - 10, b + 10)
      yield (a, b, x)
    check(forAll(gen) { (a, b, x) => interval(a, b).indexOf(x) == range(a, b).indexOf(x) })
  }

  test("lastIndexOf matches Range") {
    val gen =
      for
        (a, b) <- intervalGen
        x <- Gen.choose(a - 10, b + 10)
      yield (a, b, x)
    check(forAll(gen) { (a, b, x) => interval(a, b).lastIndexOf(x) == range(a, b).lastIndexOf(x) })
  }

  test("isDefinedAt matches Range") {
    val gen =
      for
        (a, b) <- intervalGen
        i <- Gen.choose(-5, b - a + 5)
      yield (a, b, i)
    check(forAll(gen) { (a, b, i) => interval(a, b).isDefinedAt(i) == range(a, b).isDefinedAt(i) })
  }

  test("toRange roundtrips") {
    check(forAll(intervalGen) { (a, b) => interval(a, b).toRange == range(a, b) })
  }

  test("toNumericRange roundtrips") {
    check(forAll(intervalGen) { (a, b) => interval(a, b).toNumericRange == range(a, b).toNumericRange[Int] })
  }

  test("isInclusive is always true") {
    check(forAll(intervalGen) { (a, b) => interval(a, b).isInclusive })
  }

  test("toString formats correctly") {
    check(forAll(intervalGen) { (a, b) => interval(a, b).toString == s"Interval($a..$b)" })
  }

  test("intersects is symmetric") {
    val gen =
      for
        (a, b) <- intervalGen
        (c, d) <- intervalGen
      yield (a, b, c, d)
    check(forAll(gen) { (a, b, c, d) =>
      interval(a, b).intersects(interval(c, d)) == interval(c, d).intersects(interval(a, b))
    })
  }

  test("intersects when overlapping") {
    val gen =
      for
        (a, b) <- intervalGen
        c <- Gen.choose(a, b)
        d <- Gen.choose(c, b + 100)
      yield (a, b, c, d)
    check(forAll(gen) { (a, b, c, d) => interval(a, b).intersects(interval(c, d)) })
  }

  test("does not intersect when disjoint") {
    val gen =
      for
        (a, b) <- intervalGen
        gap <- Gen.choose(1, 100)
        len <- Gen.choose(1, 100)
      yield (a, b, b + gap, b + gap + len)
    check(forAll(gen) { (a, b, c, d) => !interval(a, b).intersects(interval(c, d)) })
  }

  test("intersect is symmetric") {
    val gen =
      for
        (a, b) <- intervalGen
        (c, d) <- intervalGen
      yield (a, b, c, d)
    check(forAll(gen) { (a, b, c, d) =>
      interval(a, b).intersect(interval(c, d)) == interval(c, d).intersect(interval(a, b))
    })
  }

  test("intersect of overlapping intervals") {
    val gen =
      for
        (a, b) <- intervalGen
        c <- Gen.choose(a, b)
        d <- Gen.choose(c, b + 100)
      yield (a, b, c, d)
    check(forAll(gen) { (a, b, c, d) => interval(a, b).intersect(interval(c, d)) == interval(a max c, b min d) })
  }

  test("intersect of disjoint intervals is empty") {
    val gen =
      for
        (a, b) <- intervalGen
        gap <- Gen.choose(1, 100)
        len <- Gen.choose(1, 100)
      yield (a, b, b + gap, b + gap + len)
    check(forAll(gen) { (a, b, c, d) => interval(a, b).intersect(interval(c, d)).isEmpty })
  }

  test("ordering matches tuple ordering") {
    val gen = Gen.listOf(intervalGen).map(_.map((a, b) => interval(a, b)))
    check(forAll(gen) { intervals =>
      intervals.sorted.map(i => (i.start, i.end)) == intervals.map(i => (i.start, i.end)).sorted
    })
  }
