package org.lemon.advent.lib

import org.lemon.advent.*
import org.scalacheck.{Arbitrary, Shrink, Gen}
import org.scalacheck.Prop.*
import org.scalacheck.Shrink.*
import org.scalacheck.Arbitrary.*

given Arbitrary[Range] = Arbitrary(for
  i <- Gen.choose(-10000, 10000)
  j <- Gen.choose(-10000, 10000)
  if i != j
yield i to j by (j - i).sign)

given Shrink[Range] = Shrink { case range: Range =>
  shrink((range.start, range.end))
    .filter(_ != _)
    .map((start, end) => (start min end) to (start max end) by (end - start).sign)
}

given Arbitrary[Interval[Int]] = Arbitrary(for
  i <- Gen.choose(-10000, 10000)
  j <- Gen.choose(i, 10000)
yield Interval(i, j))

given Shrink[Interval[Int]] = Shrink { case interval: Interval[Int] =>
  shrink((interval.start, interval.end))
    .filter((s, e) => s <= e)
    .map(Interval(_, _))
}

given Arbitrary[Diet[Int]] = Arbitrary(for ranges <- Gen.listOf[Range](arbitrary[Range]) yield Diet.fromRanges(ranges))

given Shrink[Diet[Int]] = Shrink { case diet: Diet[Int] =>
  shrink(diet.toRanges).map(Diet.fromRanges)
}

class EmptyDietTest extends UnitTest:
  test("contains nothing") {
    check((n: Int) => !Diet.empty[Int].contains(n))
  }

  test("contains no range") {
    check((range: Range) => !Diet.empty[Int].contains(range.min, range.max))
  }

  test("contains no Range object") {
    check((range: Range) => !Diet.empty[Int].contains(range))
  }

  test("contains empty range") {
    check((n: Int) => n > Int.MinValue ==> Diet.empty[Int].contains(n, n - 1))
  }

  test("contains empty Range object") {
    check((n: Int) => Diet.empty[Int].contains(n until n))
  }

  test("isEmpty") {
    Diet.empty[Int].isEmpty shouldBe true
  }

  test("not nonEmpty") {
    Diet.empty[Int].nonEmpty shouldBe false
  }

  test("size is 0") {
    Diet.empty[Int].size shouldBe 0
  }

  test("adding value produces singleton diet") {
    check((n: Int) => Diet.empty[Int] + n == Diet(n))
  }

  test("adding empty range is empty diet") {
    check((n: Int) => Diet.empty[Int] == Diet(n until n))
  }

  test("adding range produces diet with single interval") {
    check((range: Range) => Diet.empty[Int] + range == Diet(range))
  }

  test("adding Interval produces diet with single interval") {
    check((interval: Interval[Int]) => Diet.empty[Int] + interval == Diet(interval.start to interval.end))
  }

  test("removing value does nothing") {
    check((n: Int) => (Diet.empty[Int] - n) == Diet.empty[Int])
  }

  test("removing range does nothing") {
    check((range: Range) => (Diet.empty[Int] - range) == Diet.empty[Int])
  }

  test("removing Interval does nothing") {
    check((interval: Interval[Int]) => (Diet.empty[Int] - interval) == Diet.empty[Int])
  }

  test("union is rhs") {
    check((rhs: Diet[Int]) => (Diet.empty[Int] ++ rhs) == rhs)
  }

  test("intersect is empty") {
    check((rhs: Diet[Int]) => (Diet.empty[Int] & rhs) == Diet.empty[Int])
  }

  test("diff is empty") {
    check((rhs: Diet[Int]) => (Diet.empty[Int] -- rhs) == Diet.empty[Int])
  }

  test("iterator is empty") {
    Diet.empty[Int].iterator.isEmpty shouldBe true
  }

  test("intervalsIterator is empty") {
    Diet.empty[Int].intervalsIterator.isEmpty shouldBe true
  }

  test("toIntervals is empty") {
    Diet.empty[Int].toIntervals shouldBe Seq.empty
  }

  test("toRanges is empty") {
    Diet.empty[Int].toRanges shouldBe Seq.empty
  }

  test("toNumericRanges is empty") {
    Diet.empty[Int].toNumericRanges shouldBe Seq.empty
  }

  test("toSeq is empty") {
    Diet.empty[Int].toSeq shouldBe Seq.empty
  }

  test("toSet is empty") {
    Diet.empty[Int].toSet shouldBe Set.empty
  }

  test("min throws NoSuchElementException") {
    a[NoSuchElementException] should be thrownBy Diet.empty[Int].min
  }

  test("minOption is None") {
    Diet.empty[Int].minOption shouldBe None
  }

  test("max throws NoSuchElementException") {
    a[NoSuchElementException] should be thrownBy Diet.empty[Int].max
  }

  test("maxOption is None") {
    Diet.empty[Int].maxOption shouldBe None
  }

  test("toString is Diet()") {
    Diet.empty[Int].toString shouldBe "Diet()"
  }

class SingletonValueDietTest extends UnitTest:
  given Arbitrary[Int] = Arbitrary(Gen.choose(-10000, 10000))

  val disjointDiet =
    for
      rhs <- arbitrary[Diet[Int]]
      n <- (rhs.minOption, rhs.maxOption) match
        case (Some(min), Some(max)) => Gen.oneOf(Gen.choose(Int.MinValue, min - 2), Gen.choose(max + 2, Int.MaxValue))
        case _ => arbitrary[Int]
    yield (n, rhs)

  test("contains the element") {
    check((n: Int) => Diet(n).contains(n))
  }

  test("contains the element as range") {
    check((n: Int) => Diet(n).contains(n, n))
  }

  test("does not contain other elements") {
    check((n: Int, m: Int) => n != m ==> !Diet(n).contains(m))
  }

  test("does not contain range extending beyond element") {
    check((n: Int) => !Diet(n).contains(n - 1, n) && !Diet(n).contains(n, n + 1) && !Diet(n).contains(n - 1, n + 1))
  }

  test("not isEmpty") {
    check((n: Int) => !Diet(n).isEmpty)
  }

  test("nonEmpty") {
    check((n: Int) => Diet(n).nonEmpty)
  }

  test("size is 1") {
    check((n: Int) => Diet(n).size == 1)
  }

  test("adding same value is idempotent") {
    check((n: Int) => (Diet(n) + n) == Diet(n))
  }

  test("adding adjacent values produces single interval") {
    check((n: Int, above: Boolean) =>
      val m = if above then n + 1 else n - 1
      (Diet(n) + m) == Diet(n to m by (m - n).sign)
    )
  }

  test("adding disjoint value produces multiple intervals") {
    check((n: Int, m: Int) =>
      (n - m).abs > 2 ==> ((Diet(n) + m).toIntervals == Seq(Interval(n, n), Interval(m, m)).sorted)
    )
  }

  test("adding overlapping range produces diet with single interval") {
    val gen =
      for
        range <- arbitrary[Range]
        n <- Gen.choose(range.min, range.max)
      yield (range, n)
    check(forAll(gen) { (range, n) => (Diet(n) + range) == Diet(range) })
  }

  test("adding disjoint range produces multiple intervals") {
    check((range: Range, n: Int) =>
      n < range.min || n > range.max ==> ((Diet(n) + range).toIntervals == Seq(
        Interval(n, n),
        Interval(range.min, range.max)
      ).sorted)
    )
  }

  test("removing value gives empty diet") {
    check((n: Int) => (Diet(n) - n) == Diet.empty[Int])
  }

  test("removing disjoint value does nothing") {
    check((n: Int, m: Int) => n != m ==> ((Diet(n) - m) == Diet(n)))
  }

  test("removing overlapping range gives empty diet") {
    val gen =
      for
        range <- arbitrary[Range]
        n <- Gen.choose(range.min, range.max)
      yield (range, n)
    check(forAll(gen) { (range, n) => (Diet(n) - range) == Diet.empty[Int] })
  }

  test("removing disjoint range does nothing") {
    check((range: Range, n: Int) =>
      n < range.min || n > range.max ==> ((Diet(n) - range) == Diet(n))
    )
  }

  test("union with empty is self") {
    check((n: Int) => (Diet(n) ++ Diet.empty[Int]) == Diet(n))
  }

  test("union with self is self") {
    check((n: Int) => (Diet(n) ++ Diet(n)) == Diet(n))
  }

  test("union with rhs contains both") {
    check((n: Int, rhs: Diet[Int]) => {
      val union = Diet(n) ++ rhs
      union.contains(n) && rhs.toSet.forall(union.contains)
    })
  }

  test("intersect with empty is empty") {
    check((n: Int) => (Diet(n) & Diet.empty[Int]) == Diet.empty[Int])
  }

  test("intersect with self is self") {
    check((n: Int) => (Diet(n) & Diet(n)) == Diet(n))
  }

  test("intersect with disjoint rhs is empty") {
    check(forAll(disjointDiet) { (n, rhs) => (Diet(n) & rhs) == Diet.empty[Int] })
  }

  test("intersect with overlapping rhs does nothing") {
    check((n: Int, rhs: Diet[Int]) =>
      rhs.contains(n) ==> ((Diet(n) & rhs) == Diet(n))
    )
  }

  test("diff with empty is self") {
    check((n: Int) => (Diet(n) -- Diet.empty[Int]) == Diet(n))
  }

  test("diff with self is empty") {
    check((n: Int) => (Diet(n) -- Diet(n)) == Diet.empty[Int])
  }

  test("diff with disjoint rhs is self") {
    check(forAll(disjointDiet) { (n, rhs) => (Diet(n) -- rhs) == Diet(n) })
  }

  test("diff with overlapping gives empty diet") {
    check((n: Int, rhs: Diet[Int]) =>
      rhs.contains(n) ==> ((Diet(n) -- rhs) == Diet.empty[Int])
    )
  }

  test("iterator is single element") {
    check((n: Int) => Diet(n).iterator.toSeq == Seq(n))
  }

  test("intervalsIterator is single interval") {
    check((n: Int) => Diet(n).intervalsIterator.toSeq == Seq((n, n)))
  }

  test("toIntervals is single element") {
    check((n: Int) => Diet(n).toIntervals == Seq(Interval(n, n)))
  }

  test("toRanges is single element range") {
    check((n: Int) => Diet(n).toRanges == Seq(n to n))
  }

  test("toNumericRanges is single element range") {
    check((n: Int) => Diet(n).toNumericRanges == Seq(n to n))
  }

  test("toSeq is single element") {
    check((n: Int) => Diet(n).toSeq == Seq(n))
  }

  test("toSet is single element") {
    check((n: Int) => Diet(n).toSet == Set(n))
  }

  test("min is the element") {
    check((n: Int) => Diet(n).min == n)
  }

  test("minOption is Some(element)") {
    check((n: Int) => Diet(n).minOption == Some(n))
  }

  test("max is the element") {
    check((n: Int) => Diet(n).max == n)
  }

  test("maxOption is Some(element)") {
    check((n: Int) => Diet(n).maxOption == Some(n))
  }

  test("toString formats single value") {
    check((n: Int) => Diet(n).toString == s"Diet($n)")
  }

class SingletonIntervalDietTest extends UnitTest:
  val disjointValue =
    for
      range <- arbitrary[Range]
      n <- Gen.oneOf(Gen.choose(Int.MinValue, range.min - 2), Gen.choose(range.max + 2, Int.MaxValue))
    yield (range, n)

  val overlappingValue =
    for
      range <- arbitrary[Range]
      n <- Gen.choose(range.min, range.max)
    yield (range, n)

  val disjointRange =
    for
      range <- arbitrary[Range]
      gap <- Gen.choose(2, 1000) // gap >= 2 so ranges aren't adjacent
      above <- Gen.oneOf(true, false)
      len <- Gen.choose(0, 100)
      other =
        if above then (range.max + gap) to (range.max + gap + len)
        else (range.min - gap - len) to (range.min - gap)
    yield (range, other)

  val overlappingRange =
    for
      range <- arbitrary[Range]
      // generate a range that definitely overlaps by picking points within the original range
      size <- Gen.choose(1, 100)
      start <- Gen.choose(range.min - size, range.max)
      end <- Gen.choose(start max range.min, range.max + size)
    yield (range, start to end)

  test("contains all elements in range") {
    check((range: Range) => range.forall(Diet(range).contains))
  }

  test("contains entire range") {
    check((range: Range) => Diet(range).contains(range.min, range.max))
  }

  test("contains entire Range object") {
    check((range: Range) => Diet(range).contains(range))
  }

  test("contains entire Interval object") {
    check((range: Range) => Diet(range).contains(Interval(range.min, range.max)))
  }

  test("contains subranges") {
    val gen =
      for
        range <- arbitrary[Range].suchThat(_.size >= 2)
        start <- Gen.choose(range.min, range.max)
        end <- Gen.choose(start, range.max)
      yield (range, start, end)
    check(forAll(gen) { (range, start, end) => Diet(range).contains(start, end) })
  }

  test("does not contain range extending before") {
    check((range: Range) => !Diet(range).contains(range.min - 1, range.max))
  }

  test("does not contain range extending after") {
    check((range: Range) => !Diet(range).contains(range.min, range.max + 1))
  }

  test("does not contain elements outside range") {
    check(forAll(disjointValue) { (range, n) => !Diet(range).contains(n) })
  }

  test("does not contain disjoint range") {
    check(forAll(disjointRange) { (range, other) => !Diet(range).contains(other.min, other.max) })
  }

  test("does not contain disjoint Range object") {
    check(forAll(disjointRange) { (range, other) => !Diet(range).contains(other) })
  }

  test("does not contain disjoint Interval object") {
    check(forAll(disjointRange) { (range, other) => !Diet(range).contains(Interval(other.min, other.max)) })
  }

  test("not isEmpty") {
    check((range: Range) => !Diet(range).isEmpty)
  }

  test("nonEmpty") {
    check((range: Range) => Diet(range).nonEmpty)
  }

  test("size matches range size") {
    check((range: Range) => Diet(range).size == range.size)
  }

  test("adding element already in range is idempotent") {
    check(forAll(overlappingValue) { (range, n) => (Diet(range) + n) == Diet(range) })
  }

  test("adding adjacent value extends interval") {
    check((range: Range, above: Boolean) =>
      val n = if above then range.max + 1 else range.min - 1
      (Diet(range) + n) == Diet((range.min min n) to (range.max max n))
    )
  }

  test("adding disjoint value produces multiple intervals") {
    check(forAll(disjointValue) { (range, n) =>
      (Diet(range) + n).toIntervals == Seq(Interval(range.min, range.max), Interval(n, n)).sorted
    })
  }

  test("adding overlapping range merges intervals") {
    check(forAll(overlappingRange) { (range, other) =>
      val diet = Diet(range) + other
      diet.toIntervals == Seq(Interval(range.min min other.min, range.max max other.max))
    })
  }

  test("adding disjoint range produces multiple intervals") {
    check(forAll(disjointRange) { (range, other) =>
      (Diet(range) + other).toIntervals == Seq(Interval(range.min, range.max), Interval(other.min, other.max)).sorted
    })
  }

  test("adding disjoint Interval produces multiple intervals") {
    check(forAll(disjointRange) { (range, other) =>
      (Diet(range) + Interval(other.min, other.max)).toIntervals == Seq(
        Interval(range.min, range.max),
        Interval(other.min, other.max)
      ).sorted
    })
  }

  test("adding overlapping Interval merges intervals") {
    check(forAll(overlappingRange) { (range, other) =>
      val diet = Diet(range) + Interval(other.min, other.max)
      diet.toIntervals == Seq(Interval(range.min min other.min, range.max max other.max))
    })
  }

  test("removing element from middle splits interval") {
    val gen =
      for
        range <- arbitrary[Range].suchThat(_.size > 2)
        n <- Gen.choose(range.min + 1, range.max - 1)
      yield (range, n)
    check(forAll(gen) { (range, n) =>
      val diet = Diet(range) - n
      diet.toIntervals == Seq(Interval(range.min, n - 1), Interval(n + 1, range.max))
    })
  }

  test("removing element from start shrinks interval") {
    check((range: Range) => (Diet(range) - range.min).toIntervals == Seq(Interval(range.min + 1, range.max)))
  }

  test("removing element from end shrinks interval") {
    check((range: Range) => (Diet(range) - range.max).toIntervals == Seq(Interval(range.min, range.max - 1)))
  }

  test("removing disjoint value does nothing") {
    check(forAll(disjointValue) { (range, n) => (Diet(range) - n) == Diet(range) })
  }

  test("removing overlapping range removes overlap") {
    check(forAll(overlappingRange) { (range, other) =>
      val diet = Diet(range) - other
      diet.toSet == (range.toSet -- other.toSet)
    })
  }

  test("removing disjoint range does nothing") {
    check(forAll(disjointRange) { (range, other) => (Diet(range) - other) == Diet(range) })
  }

  test("removing entire range gives empty diet") {
    check((range: Range) => (Diet(range) - range) == Diet.empty[Int])
  }

  test("removing entire Interval gives empty diet") {
    check((range: Range) => (Diet(range) - Interval(range.min, range.max)) == Diet.empty[Int])
  }

  test("removing overlapping Interval removes overlap") {
    check(forAll(overlappingRange) { (range, other) =>
      val diet = Diet(range) - Interval(other.min, other.max)
      diet.toSet == (range.toSet -- other.toSet)
    })
  }

  test("union with empty is self") {
    check((range: Range) => (Diet(range) ++ Diet.empty[Int]) == Diet(range))
  }

  test("union with self is self") {
    check((range: Range) => (Diet(range) ++ Diet(range)) == Diet(range))
  }

  test("union with rhs contains both") {
    check((range: Range, rhs: Diet[Int]) => {
      val union = Diet(range) ++ rhs
      range.forall(union.contains) && rhs.toSet.forall(union.contains)
    })
  }

  test("intersect with empty is empty") {
    check((range: Range) => (Diet(range) & Diet.empty[Int]) == Diet.empty[Int])
  }

  test("intersect with self is self") {
    check((range: Range) => (Diet(range) & Diet(range)) == Diet(range))
  }

  test("intersect with disjoint range is empty") {
    check(forAll(disjointRange) { (range, other) => (Diet(range) & Diet(other)) == Diet.empty[Int] })
  }

  test("intersect with overlapping range gives overlap") {
    check(forAll(overlappingRange) { (range, other) =>
      (Diet(range) & Diet(other)).toSet == (range.toSet & other.toSet)
    })
  }

  test("diff with empty is self") {
    check((range: Range) => (Diet(range) -- Diet.empty[Int]) == Diet(range))
  }

  test("diff with self is empty") {
    check((range: Range) => (Diet(range) -- Diet(range)) == Diet.empty[Int])
  }

  test("diff with disjoint range is self") {
    check(forAll(disjointRange) { (range, other) => (Diet(range) -- Diet(other)) == Diet(range) })
  }

  test("diff with overlapping range removes overlap") {
    check(forAll(overlappingRange) { (range, other) =>
      (Diet(range) -- Diet(other)).toSet == (range.toSet -- other.toSet)
    })
  }

  test("iterator yields all values in order") {
    check((range: Range) => Diet(range).iterator.toSeq == range.sorted)
  }

  test("intervalsIterator is single interval") {
    check((range: Range) => Diet(range).intervalsIterator.toSeq == Seq((range.min, range.max)))
  }

  test("toIntervals is single interval") {
    check((range: Range) => Diet(range).toIntervals == Seq(Interval(range.min, range.max)))
  }

  test("toRanges is single range") {
    check((range: Range) => Diet(range).toRanges == Seq(range.min to range.max))
  }

  test("toNumericRanges is single range") {
    check((range: Range) => Diet(range).toNumericRanges == Seq(range.min to range.max))
  }

  test("toSeq matches sorted range") {
    check((range: Range) => Diet(range).toSeq == range.sorted)
  }

  test("toSet matches range set") {
    check((range: Range) => Diet(range).toSet == range.toSet)
  }

  test("min is range min") {
    check((range: Range) => Diet(range).min == range.min)
  }

  test("minOption is Some(range min)") {
    check((range: Range) => Diet(range).minOption == Some(range.min))
  }

  test("max is range max") {
    check((range: Range) => Diet(range).max == range.max)
  }

  test("maxOption is Some(range max)") {
    check((range: Range) => Diet(range).maxOption == Some(range.max))
  }

  test("toString formats interval") {
    check((range: Range) => Diet(range).toString == s"Diet(${range.min}..${range.max})")
  }

  test("constructor with Interval matches constructor with Range") {
    check((range: Range) => Diet(Interval(range.min, range.max)) == Diet(range))
  }

  test("apply(Interval) returns true for contained interval") {
    check((range: Range) => Diet(range)(Interval(range.min, range.max)))
  }

  test("apply(Interval) returns false for non-contained interval") {
    check(forAll(disjointRange) { (range, other) => !Diet(range)(Interval(other.min, other.max)) })
  }

class MultipleValueDietTest extends UnitTest:
  // generates 2+ disjoint values with gaps >= 2 between them
  val disjointValuesGen: Gen[Seq[Int]] =
    for
      count <- Gen.choose(2, 10)
      first <- Gen.choose(-1000, 1000)
      gaps <- Gen.listOfN(count - 1, Gen.choose(2, 100))
    yield gaps.scanLeft(first)(_ + _)

  given Arbitrary[Seq[Int]] = Arbitrary(disjointValuesGen)
  given Shrink[Seq[Int]] = Shrink.shrinkAny // disable shrinking to preserve 2+ values

  val disjointValue =
    for
      values <- disjointValuesGen
      n <- Gen.oneOf(
        Gen.choose(Int.MinValue, values.min - 2),
        Gen.choose(values.max + 2, Int.MaxValue)
      )
    yield (values, n)

  val overlappingValue =
    for
      values <- disjointValuesGen
      n <- Gen.oneOf(values)
    yield (values, n)

  test("contains all elements") {
    check((values: Seq[Int]) => values.forall(Diet(values).contains))
  }

  test("does not contain elements between values") {
    check(forAll(disjointValue) { (values, n) => !Diet(values).contains(n) })
  }

  test("not isEmpty") {
    check((values: Seq[Int]) => !Diet(values).isEmpty)
  }

  test("nonEmpty") {
    check((values: Seq[Int]) => Diet(values).nonEmpty)
  }

  test("size equals count of values") {
    check((values: Seq[Int]) => Diet(values).size == values.size)
  }

  test("adding existing value is idempotent") {
    check(forAll(overlappingValue) { (values, n) => Diet(values) + n == Diet(values) })
  }

  test("adding value that merges two intervals reduces interval count") {
    // generate values where at least one gap is exactly 2
    val gen =
      for
        count <- Gen.choose(2, 10)
        first <- Gen.choose(-1000, 1000)
        mergeIndex <- Gen.choose(0, count - 2)
        gaps <- Gen.sequence[Seq[Int], Int](
          (0 until count - 1).map(i => if i == mergeIndex then Gen.const(2) else Gen.choose(2, 100))
        )
        values = gaps.scanLeft(first)(_ + _)
        mergeValue = values(mergeIndex) + 1
      yield (values, mergeValue)
    check(forAll(gen) { (values, n) =>
      val diet = Diet(values)
      (diet + n).toIntervals.size == diet.toIntervals.size - 1
    })
  }

  test("adding disjoint value increases interval count") {
    check(forAll(disjointValue) { (values, n) =>
      val diet = Diet(values)
      (diet + n).toIntervals.size == diet.toIntervals.size + 1
    })
  }

  test("removing existing value decreases size") {
    check(forAll(overlappingValue) { (values, n) =>
      val diet = Diet(values)
      (diet - n).size == diet.size - 1
    })
  }

  test("removing disjoint value does nothing") {
    check(forAll(disjointValue) { (values, n) => Diet(values) - n == Diet(values) })
  }

  test("union with empty is self") {
    check((values: Seq[Int]) => Diet(values) ++ Diet.empty[Int] == Diet(values))
  }

  test("union with self is self") {
    check((values: Seq[Int]) => Diet(values) ++ Diet(values) == Diet(values))
  }

  test("union with rhs contains both") {
    check((values: Seq[Int], rhs: Diet[Int]) => {
      val diet = Diet(values)
      val union = diet ++ rhs
      values.forall(union.contains) && rhs.toSet.forall(union.contains)
    })
  }

  test("intersect with empty is empty") {
    check((values: Seq[Int]) =>
      (Diet(values) & Diet.empty[Int]) == Diet.empty[Int]
    )
  }

  test("intersect with self is self") {
    check((values: Seq[Int]) =>
      (Diet(values) & Diet(values)) == Diet(values)
    )
  }

  test("intersect with overlapping value gives that value") {
    check(forAll(overlappingValue) { (values, n) =>
      (Diet(values) & Diet(n)) == Diet(n)
    })
  }

  test("intersect with disjoint value is empty") {
    check(forAll(disjointValue) { (values, n) =>
      (Diet(values) & Diet(n)) == Diet.empty[Int]
    })
  }

  test("diff with empty is self") {
    check((values: Seq[Int]) =>
      Diet(values) -- Diet.empty[Int] == Diet(values)
    )
  }

  test("diff with self is empty") {
    check((values: Seq[Int]) =>
      (Diet(values) -- Diet(values)) == Diet.empty[Int]
    )
  }

  test("diff with overlapping value removes it") {
    check(forAll(overlappingValue) { (values, n) =>
      val diet = Diet(values)
      (diet -- Diet(n)).toSet == (diet.toSet - n)
    })
  }

  test("diff with disjoint value is self") {
    check(forAll(disjointValue) { (values, n) => Diet(values) -- Diet(n) == Diet(values) })
  }

  test("iterator yields all values in order") {
    check((values: Seq[Int]) => Diet(values).iterator.toSeq == values.sorted)
  }

  test("intervalsIterator yields all singleton intervals") {
    check((values: Seq[Int]) => Diet(values).intervalsIterator.toSeq == values.sorted.map(v => (v, v)))
  }

  test("toIntervals yields all singleton intervals") {
    check((values: Seq[Int]) => Diet(values).toIntervals == values.sorted.map(v => Interval(v, v)))
  }

  test("toRanges yields all singleton ranges") {
    check((values: Seq[Int]) => Diet(values).toRanges == values.sorted.map(v => v to v))
  }

  test("toNumericRanges yields all singleton ranges") {
    check((values: Seq[Int]) => Diet(values).toNumericRanges == values.sorted.map(v => v to v))
  }

  test("toSeq matches sorted values") {
    check((values: Seq[Int]) => Diet(values).toSeq == values.sorted)
  }

  test("toSet matches value set") {
    check((values: Seq[Int]) => Diet(values).toSet == values.toSet)
  }

  test("min is smallest value") {
    check((values: Seq[Int]) => Diet(values).min == values.min)
  }

  test("minOption is Some(smallest value)") {
    check((values: Seq[Int]) => Diet(values).minOption == Some(values.min))
  }

  test("max is largest value") {
    check((values: Seq[Int]) => Diet(values).max == values.max)
  }

  test("maxOption is Some(largest value)") {
    check((values: Seq[Int]) => Diet(values).maxOption == Some(values.max))
  }

  test("toString formats all values") {
    check((values: Seq[Int]) => Diet(values).toString == values.sorted.mkString("Diet(", ", ", ")"))
  }

class MultipleIntervalDietTest extends UnitTest:
  // generates 2+ disjoint ranges with gaps >= 2 between them
  val disjointRangesGen: Gen[Seq[Range]] =
    for
      count <- Gen.choose(2, 5)
      first <- Gen.choose(-1000, 1000)
      specs <- Gen.listOfN(count, for len <- Gen.choose(1, 50); gap <- Gen.choose(2, 100) yield (len, gap))
    yield specs.scanLeft((first, first)) { case ((_, prevEnd), (len, gap)) =>
      val start = prevEnd + gap
      (start, start + len - 1)
    }.tail.map((s, e) => s to e)

  given Arbitrary[Seq[Range]] = Arbitrary(disjointRangesGen)
  given Shrink[Seq[Range]] = Shrink.shrinkAny // disable shrinking to preserve 2+ ranges

  val disjointValue =
    for
      ranges <- disjointRangesGen
      n <- Gen.oneOf(
        Gen.choose(Int.MinValue, ranges.head.min - 2),
        Gen.choose(ranges.last.max + 2, Int.MaxValue)
      )
    yield (ranges, n)

  val overlappingValue =
    for
      ranges <- disjointRangesGen
      range <- Gen.oneOf(ranges)
      n <- Gen.choose(range.min, range.max)
    yield (ranges, n)

  val disjointRange =
    for
      ranges <- disjointRangesGen
      gap <- Gen.choose(2, 100)
      len <- Gen.choose(1, 50)
      above <- Gen.oneOf(true, false)
      other =
        if above then (ranges.last.max + gap) to (ranges.last.max + gap + len - 1)
        else (ranges.head.min - gap - len + 1) to (ranges.head.min - gap)
    yield (ranges, other)

  val overlappingRange =
    for
      ranges <- disjointRangesGen
      range <- Gen.oneOf(ranges)
      size <- Gen.choose(1, 20)
      start <- Gen.choose(range.min - size, range.max)
      end <- Gen.choose(start max range.min, range.max + size)
    yield (ranges, start to end)

  test("contains all elements in all ranges") {
    check((ranges: Seq[Range]) => ranges.flatMap(_.toSet).toSet.forall(Diet.fromRanges(ranges).contains))
  }

  test("contains each individual range") {
    check((ranges: Seq[Range]) => ranges.forall(r => Diet.fromRanges(ranges).contains(r.min, r.max)))
  }

  test("contains each individual Range object") {
    check((ranges: Seq[Range]) => ranges.forall(r => Diet.fromRanges(ranges).contains(r)))
  }

  test("contains subranges within each interval") {
    val gen =
      for
        ranges <- disjointRangesGen
        range <- Gen.oneOf(ranges).suchThat(_.size >= 2)
        start <- Gen.choose(range.min, range.max)
        end <- Gen.choose(start, range.max)
      yield (ranges, start, end)
    check(forAll(gen) { (ranges, start, end) => Diet.fromRanges(ranges).contains(start, end) })
  }

  test("does not contain range spanning gap between intervals") {
    val gen =
      for
        ranges <- disjointRangesGen.suchThat(_.size >= 2)
        idx <- Gen.choose(0, ranges.size - 2)
      yield (ranges, ranges(idx).max, ranges(idx + 1).min)
    check(forAll(gen) { (ranges, gapStart, gapEnd) =>
      !Diet.fromRanges(ranges).contains(gapStart, gapEnd)
    })
  }

  test("does not contain elements outside ranges") {
    check(forAll(disjointValue) { (ranges, n) => !Diet.fromRanges(ranges).contains(n) })
  }

  test("does not contain disjoint range") {
    check(forAll(disjointRange) { (ranges, other) => !Diet.fromRanges(ranges).contains(other.min, other.max) })
  }

  test("does not contain disjoint Range object") {
    check(forAll(disjointRange) { (ranges, other) => !Diet.fromRanges(ranges).contains(other) })
  }

  test("not isEmpty") {
    check((ranges: Seq[Range]) => !Diet.fromRanges(ranges).isEmpty)
  }

  test("nonEmpty") {
    check((ranges: Seq[Range]) => Diet.fromRanges(ranges).nonEmpty)
  }

  test("size equals total of all range sizes") {
    check((ranges: Seq[Range]) => Diet.fromRanges(ranges).size == ranges.map(_.size).sum)
  }

  test("adding existing value is idempotent") {
    check(forAll(overlappingValue) { (ranges, n) => Diet.fromRanges(ranges) + n == Diet.fromRanges(ranges) })
  }

  test("adding disjoint value increases interval count") {
    check(forAll(disjointValue) { (ranges, n) =>
      val diet = Diet.fromRanges(ranges)
      (diet + n).toIntervals.size == diet.toIntervals.size + 1
    })
  }

  test("adding disjoint range increases interval count") {
    check(forAll(disjointRange) { (ranges, other) =>
      val diet = Diet.fromRanges(ranges)
      (diet + other).toIntervals.size == diet.toIntervals.size + 1
    })
  }

  test("adding overlapping range may merge intervals") {
    check(forAll(overlappingRange) { (ranges, other) =>
      val diet = Diet.fromRanges(ranges)
      (diet + other).toIntervals.size <= diet.toIntervals.size
    })
  }

  test("removing existing value decreases size") {
    check(forAll(overlappingValue) { (ranges, n) =>
      val diet = Diet.fromRanges(ranges)
      (diet - n).size == diet.size - 1
    })
  }

  test("removing disjoint value does nothing") {
    check(forAll(disjointValue) { (ranges, n) => Diet.fromRanges(ranges) - n == Diet.fromRanges(ranges) })
  }

  test("removing disjoint range does nothing") {
    check(forAll(disjointRange) { (ranges, other) => Diet.fromRanges(ranges) - other == Diet.fromRanges(ranges) })
  }

  test("removing overlapping range removes overlap") {
    check(forAll(overlappingRange) { (ranges, other) =>
      val diet = Diet.fromRanges(ranges)
      (diet - other).toSet == (ranges.flatMap(_.toSet).toSet -- other.toSet)
    })
  }

  test("union with empty is self") {
    check((ranges: Seq[Range]) => Diet.fromRanges(ranges) ++ Diet.empty[Int] == Diet.fromRanges(ranges))
  }

  test("union with self is self") {
    check((ranges: Seq[Range]) => Diet.fromRanges(ranges) ++ Diet.fromRanges(ranges) == Diet.fromRanges(ranges))
  }

  test("union with rhs contains both") {
    check((ranges: Seq[Range], rhs: Diet[Int]) => {
      val diet = Diet.fromRanges(ranges)
      val union = diet ++ rhs
      ranges.flatMap(_.toSet).toSet.forall(union.contains) && rhs.toSet.forall(union.contains)
    })
  }

  test("intersect with empty is empty") {
    check((ranges: Seq[Range]) => (Diet.fromRanges(ranges) & Diet.empty[Int]) == Diet.empty[Int])
  }

  test("intersect with self is self") {
    check((ranges: Seq[Range]) => (Diet.fromRanges(ranges) & Diet.fromRanges(ranges)) == Diet.fromRanges(ranges))
  }

  test("intersect with overlapping range gives overlap") {
    check(forAll(overlappingRange) { (ranges, other) =>
      (Diet.fromRanges(ranges) & Diet(other)).toSet == (ranges.flatMap(_.toSet).toSet & other.toSet)
    })
  }

  test("intersect with disjoint range is empty") {
    check(forAll(disjointRange) { (ranges, other) => (Diet.fromRanges(ranges) & Diet(other)) == Diet.empty[Int] })
  }

  test("diff with empty is self") {
    check((ranges: Seq[Range]) => Diet.fromRanges(ranges) -- Diet.empty[Int] == Diet.fromRanges(ranges))
  }

  test("diff with self is empty") {
    check((ranges: Seq[Range]) => (Diet.fromRanges(ranges) -- Diet.fromRanges(ranges)) == Diet.empty[Int])
  }

  test("diff with overlapping range removes overlap") {
    check(forAll(overlappingRange) { (ranges, other) =>
      (Diet.fromRanges(ranges) -- Diet(other)).toSet == (ranges.flatMap(_.toSet).toSet -- other.toSet)
    })
  }

  test("diff with disjoint range is self") {
    check(forAll(disjointRange) { (ranges, other) =>
      Diet.fromRanges(ranges) -- Diet(other) == Diet.fromRanges(ranges)
    })
  }

  test("iterator yields all values in order") {
    check((ranges: Seq[Range]) => Diet.fromRanges(ranges).iterator.toSeq == ranges.flatMap(_.toSet).toSet.toSeq.sorted)
  }

  test("intervalsIterator yields all intervals") {
    check((ranges: Seq[Range]) => Diet.fromRanges(ranges).intervalsIterator.toSeq == ranges.map(r => (r.min, r.max)))
  }

  test("toIntervals yields all intervals") {
    check((ranges: Seq[Range]) => Diet.fromRanges(ranges).toIntervals == ranges.map(r => Interval(r.min, r.max)))
  }

  test("toRanges yields all ranges") {
    check((ranges: Seq[Range]) => Diet.fromRanges(ranges).toRanges == ranges.map(r => r.min to r.max))
  }

  test("toNumericRanges yields all ranges") {
    check((ranges: Seq[Range]) => Diet.fromRanges(ranges).toNumericRanges == ranges.map(r => r.min to r.max))
  }

  test("toSeq matches all values sorted") {
    check((ranges: Seq[Range]) => Diet.fromRanges(ranges).toSeq == ranges.flatMap(_.toSet).toSet.toSeq.sorted)
  }

  test("toSet matches all values") {
    check((ranges: Seq[Range]) => Diet.fromRanges(ranges).toSet == ranges.flatMap(_.toSet).toSet)
  }

  test("min is smallest value") {
    check((ranges: Seq[Range]) => Diet.fromRanges(ranges).min == ranges.head.min)
  }

  test("minOption is Some(smallest value)") {
    check((ranges: Seq[Range]) => Diet.fromRanges(ranges).minOption == Some(ranges.head.min))
  }

  test("max is largest value") {
    check((ranges: Seq[Range]) => Diet.fromRanges(ranges).max == ranges.last.max)
  }

  test("maxOption is Some(largest value)") {
    check((ranges: Seq[Range]) => Diet.fromRanges(ranges).maxOption == Some(ranges.last.max))
  }

  test("toString formats all intervals") {
    check((ranges: Seq[Range]) =>
      Diet.fromRanges(ranges).toString == ranges
        .map(r => if r.size == 1 then r.min.toString else s"${r.min}..${r.max}")
        .mkString("Diet(", ", ", ")")
    )
  }
