package org.lemon.advent.lib

import org.lemon.advent.*
import org.scalacheck.{Arbitrary, Shrink, Gen, Prop}
import org.scalacheck.Prop.*
import org.scalacheck.Shrink.*
import org.scalacheck.Arbitrary.*
import org.scalacheck.commands.Commands

import scala.util.Try

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

given Arbitrary[Diet[Int]] = Arbitrary(for ranges <- Gen.listOf[Range](arbitrary[Range]) yield Diet(ranges))

given Shrink[Diet[Int]] = Shrink { case diet: Diet[Int] =>
  shrink(diet.toRanges).map(Diet.apply)
}

def isCanonical(diet: Diet[Int]): Boolean =
  val intervals = diet.toIntervals
  intervals.forall(_.nonEmpty) && intervals.sliding2.forall((a, b) => a.end.toLong + 1 < b.start.toLong)

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

  test("adding Int.MinValue and Int.MaxValue contains both") {
    val diet = Diet.empty[Int] + Int.MinValue + Int.MaxValue
    diet.contains(Int.MinValue) shouldBe true
    diet.contains(Int.MaxValue) shouldBe true
  }

  test("adding a range up to Int.MaxValue contains all of it") {
    check((n: Int) =>
      val diet = Diet.empty[Int] + (n, Int.MaxValue)
      diet.contains(n) && diet.contains(Int.MaxValue)
    )
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
      n < range.min - 1 || n > range.max + 1 ==> ((Diet(n) + range).toIntervals == Seq(
        Interval(n, n),
        Interval(range.min, range.max)
      ).sorted)
    )
  }

  val nearby =
    for
      n <- arbitrary[Int]
      other <- Gen.choose(n - 2, n + 2)
    yield (n, other)

  test("adding Int.MinValue and Int.MaxValue contains both") {
    check((n: Int) =>
      val diet = Diet(n) + Int.MinValue + Int.MaxValue
      diet.contains(Int.MinValue) && diet.contains(Int.MaxValue) && diet.contains(n)
    )
  }

  test("adding a range up to Int.MaxValue merges the element if it is above the start") {
    check(forAll(nearby) { (n, start) =>
      val added = Diet(n) + (start, Int.MaxValue)
      isCanonical(added) && added.contains(Int.MaxValue) && added.contains(n)
    })
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

  test("union with rhs contains exactly the values of both") {
    check((n: Int, rhs: Diet[Int]) => {
      val union = Diet(n) ++ rhs
      isCanonical(union) && union.toSet == rhs.toSet + n
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

  test("intersect with a range up to Int.MaxValue keeps the element if it is at or above the start") {
    check(forAll(nearby) { (n, start) =>
      val expected = if n >= start then Set(n) else Set.empty[Int]
      (Diet(start, Int.MaxValue) & Diet(n)).toSet == expected &&
      (Diet(n) & Diet(start, Int.MaxValue)).toSet == expected
    })
  }

  test("intersect with a range down to Int.MinValue keeps the element if it is at or below the end") {
    check(forAll(nearby) { (n, end) =>
      val expected = if n <= end then Set(n) else Set.empty[Int]
      (Diet(Int.MinValue, end) & Diet(n)).toSet == expected &&
      (Diet(n) & Diet(Int.MinValue, end)).toSet == expected
    })
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
    check((n: Int) => Diet(n).intervalsIterator.toSeq == Seq(Interval(n, n)))
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

  val nearby =
    for
      range <- arbitrary[Range]
      n <- Gen.choose(range.min - 2, range.max + 2)
    yield (range, n)

  test("adding Int.MinValue and Int.MaxValue contains both") {
    check((range: Range) =>
      val diet = Diet(range) + Int.MinValue + Int.MaxValue
      diet.contains(Int.MinValue) && diet.contains(Int.MaxValue) && diet.contains(range)
    )
  }

  test("adding a range up to Int.MaxValue merges everything above its start") {
    check(forAll(nearby) { (range, start) =>
      val added = Diet(range) + (start, Int.MaxValue)
      isCanonical(added) && added.contains(Int.MaxValue) && added.contains(range)
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

  test("union with rhs contains exactly the values of both") {
    check((range: Range, rhs: Diet[Int]) => {
      val union = Diet(range) ++ rhs
      isCanonical(union) && union.toSet == range.toSet ++ rhs.toSet
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

  test("intersect with a range up to Int.MaxValue keeps the values from its start") {
    check(forAll(nearby) { (range, start) =>
      val expected = range.toSet.filter(_ >= start)
      (Diet(start, Int.MaxValue) & Diet(range)).toSet == expected &&
      (Diet(range) & Diet(start, Int.MaxValue)).toSet == expected
    })
  }

  test("intersect with a range down to Int.MinValue keeps the values up to its end") {
    check(forAll(nearby) { (range, end) =>
      val expected = range.toSet.filter(_ <= end)
      (Diet(Int.MinValue, end) & Diet(range)).toSet == expected &&
      (Diet(range) & Diet(Int.MinValue, end)).toSet == expected
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
    check((range: Range) => Diet(range).intervalsIterator.toSeq == Seq(Interval(range.min, range.max)))
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

  val nearby =
    for
      values <- disjointValuesGen
      n <- Gen.choose(values.min - 2, values.max + 2)
    yield (values, n)

  test("adding Int.MinValue and Int.MaxValue contains both") {
    check((values: Seq[Int]) =>
      val diet = Diet(values) + Int.MinValue + Int.MaxValue
      diet.contains(Int.MinValue) && diet.contains(Int.MaxValue) && values.forall(diet.contains)
    )
  }

  test("adding a range up to Int.MaxValue merges every value above its start") {
    check(forAll(nearby) { (values, start) =>
      val added = Diet(values) + (start, Int.MaxValue)
      isCanonical(added) && added.contains(Int.MaxValue) && values.forall(added.contains)
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

  test("union with rhs contains exactly the values of both") {
    check((values: Seq[Int], rhs: Diet[Int]) => {
      val union = Diet(values) ++ rhs
      isCanonical(union) && union.toSet == values.toSet ++ rhs.toSet
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

  test("intersect with a range up to Int.MaxValue keeps the values from its start") {
    check(forAll(nearby) { (values, start) =>
      val expected = values.toSet.filter(_ >= start)
      (Diet(start, Int.MaxValue) & Diet(values)).toSet == expected &&
      (Diet(values) & Diet(start, Int.MaxValue)).toSet == expected
    })
  }

  test("intersect with a range down to Int.MinValue keeps the values up to its end") {
    check(forAll(nearby) { (values, end) =>
      val expected = values.toSet.filter(_ <= end)
      (Diet(Int.MinValue, end) & Diet(values)).toSet == expected &&
      (Diet(values) & Diet(Int.MinValue, end)).toSet == expected
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
    check((values: Seq[Int]) => Diet(values).intervalsIterator.toSeq == values.sorted.map(v => Interval(v, v)))
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
    check((ranges: Seq[Range]) => ranges.flatMap(_.toSet).toSet.forall(Diet(ranges).contains))
  }

  test("contains each individual range") {
    check((ranges: Seq[Range]) => ranges.forall(r => Diet(ranges).contains(r.min, r.max)))
  }

  test("contains each individual Range object") {
    check((ranges: Seq[Range]) => ranges.forall(r => Diet(ranges).contains(r)))
  }

  test("contains subranges within each interval") {
    val gen =
      for
        ranges <- disjointRangesGen
        range <- Gen.oneOf(ranges).suchThat(_.size >= 2)
        start <- Gen.choose(range.min, range.max)
        end <- Gen.choose(start, range.max)
      yield (ranges, start, end)
    check(forAll(gen) { (ranges, start, end) => Diet(ranges).contains(start, end) })
  }

  test("does not contain range spanning gap between intervals") {
    val gen =
      for
        ranges <- disjointRangesGen.suchThat(_.size >= 2)
        idx <- Gen.choose(0, ranges.size - 2)
      yield (ranges, ranges(idx).max, ranges(idx + 1).min)
    check(forAll(gen) { (ranges, gapStart, gapEnd) =>
      !Diet(ranges).contains(gapStart, gapEnd)
    })
  }

  test("does not contain elements outside ranges") {
    check(forAll(disjointValue) { (ranges, n) => !Diet(ranges).contains(n) })
  }

  test("does not contain disjoint range") {
    check(forAll(disjointRange) { (ranges, other) => !Diet(ranges).contains(other.min, other.max) })
  }

  test("does not contain disjoint Range object") {
    check(forAll(disjointRange) { (ranges, other) => !Diet(ranges).contains(other) })
  }

  test("not isEmpty") {
    check((ranges: Seq[Range]) => !Diet(ranges).isEmpty)
  }

  test("nonEmpty") {
    check((ranges: Seq[Range]) => Diet(ranges).nonEmpty)
  }

  test("size equals total of all range sizes") {
    check((ranges: Seq[Range]) => Diet(ranges).size == ranges.map(_.size).sum)
  }

  test("adding existing value is idempotent") {
    check(forAll(overlappingValue) { (ranges, n) => Diet(ranges) + n == Diet(ranges) })
  }

  test("adding disjoint value increases interval count") {
    check(forAll(disjointValue) { (ranges, n) =>
      val diet = Diet(ranges)
      (diet + n).toIntervals.size == diet.toIntervals.size + 1
    })
  }

  test("adding disjoint range increases interval count") {
    check(forAll(disjointRange) { (ranges, other) =>
      val diet = Diet(ranges)
      (diet + other).toIntervals.size == diet.toIntervals.size + 1
    })
  }

  test("adding overlapping range merges the intervals it touches") {
    check(forAll(overlappingRange) { (ranges, other) =>
      val added = Diet(ranges) + other
      isCanonical(added) && added.toSet == ranges.flatMap(_.toSet).toSet ++ other.toSet
    })
  }

  val nearby =
    for
      ranges <- disjointRangesGen
      n <- Gen.choose(ranges.head.min - 2, ranges.last.max + 2)
    yield (ranges, n)

  test("adding Int.MinValue and Int.MaxValue contains both") {
    check((ranges: Seq[Range]) =>
      val diet = Diet(ranges) + Int.MinValue + Int.MaxValue
      diet.contains(Int.MinValue) && diet.contains(Int.MaxValue) && ranges.forall(r => diet.contains(r))
    )
  }

  test("adding a range up to Int.MaxValue merges every interval above its start") {
    check(forAll(nearby) { (ranges, start) =>
      val added = Diet(ranges) + (start, Int.MaxValue)
      isCanonical(added) && added.contains(Int.MaxValue) && ranges.forall(r => added.contains(r))
    })
  }

  test("removing existing value decreases size") {
    check(forAll(overlappingValue) { (ranges, n) =>
      val diet = Diet(ranges)
      (diet - n).size == diet.size - 1
    })
  }

  test("removing disjoint value does nothing") {
    check(forAll(disjointValue) { (ranges, n) => Diet(ranges) - n == Diet(ranges) })
  }

  test("removing disjoint range does nothing") {
    check(forAll(disjointRange) { (ranges, other) => Diet(ranges) - other == Diet(ranges) })
  }

  test("removing overlapping range removes overlap") {
    check(forAll(overlappingRange) { (ranges, other) =>
      val diet = Diet(ranges)
      (diet - other).toSet == (ranges.flatMap(_.toSet).toSet -- other.toSet)
    })
  }

  test("union with empty is self") {
    check((ranges: Seq[Range]) => Diet(ranges) ++ Diet.empty[Int] == Diet(ranges))
  }

  test("union with self is self") {
    check((ranges: Seq[Range]) => Diet(ranges) ++ Diet(ranges) == Diet(ranges))
  }

  test("union with rhs contains exactly the values of both") {
    check((ranges: Seq[Range], rhs: Diet[Int]) => {
      val union = Diet(ranges) ++ rhs
      isCanonical(union) && union.toSet == ranges.flatMap(_.toSet).toSet ++ rhs.toSet
    })
  }

  test("intersect with empty is empty") {
    check((ranges: Seq[Range]) => (Diet(ranges) & Diet.empty[Int]) == Diet.empty[Int])
  }

  test("intersect with self is self") {
    check((ranges: Seq[Range]) => (Diet(ranges) & Diet(ranges)) == Diet(ranges))
  }

  test("intersect with overlapping range gives overlap") {
    check(forAll(overlappingRange) { (ranges, other) =>
      (Diet(ranges) & Diet(other)).toSet == (ranges.flatMap(_.toSet).toSet & other.toSet)
    })
  }

  test("intersect with disjoint range is empty") {
    check(forAll(disjointRange) { (ranges, other) => (Diet(ranges) & Diet(other)) == Diet.empty[Int] })
  }

  test("intersect with a range up to Int.MaxValue keeps the values from its start") {
    check(forAll(nearby) { (ranges, start) =>
      val expected = ranges.flatMap(_.toSet).toSet.filter(_ >= start)
      (Diet(start, Int.MaxValue) & Diet(ranges)).toSet == expected &&
      (Diet(ranges) & Diet(start, Int.MaxValue)).toSet == expected
    })
  }

  test("intersect with a range down to Int.MinValue keeps the values up to its end") {
    check(forAll(nearby) { (ranges, end) =>
      val expected = ranges.flatMap(_.toSet).toSet.filter(_ <= end)
      (Diet(Int.MinValue, end) & Diet(ranges)).toSet == expected &&
      (Diet(ranges) & Diet(Int.MinValue, end)).toSet == expected
    })
  }

  test("diff with empty is self") {
    check((ranges: Seq[Range]) => Diet(ranges) -- Diet.empty[Int] == Diet(ranges))
  }

  test("diff with self is empty") {
    check((ranges: Seq[Range]) => (Diet(ranges) -- Diet(ranges)) == Diet.empty[Int])
  }

  test("diff with overlapping range removes overlap") {
    check(forAll(overlappingRange) { (ranges, other) =>
      (Diet(ranges) -- Diet(other)).toSet == (ranges.flatMap(_.toSet).toSet -- other.toSet)
    })
  }

  test("diff with disjoint range is self") {
    check(forAll(disjointRange) { (ranges, other) =>
      Diet(ranges) -- Diet(other) == Diet(ranges)
    })
  }

  test("iterator yields all values in order") {
    check((ranges: Seq[Range]) => Diet(ranges).iterator.toSeq == ranges.flatMap(_.toSet).toSet.toSeq.sorted)
  }

  test("intervalsIterator yields all intervals") {
    check((ranges: Seq[Range]) => Diet(ranges).intervalsIterator.toSeq == ranges.map(r => Interval(r.min, r.max)))
  }

  test("toIntervals yields all intervals") {
    check((ranges: Seq[Range]) => Diet(ranges).toIntervals == ranges.map(r => Interval(r.min, r.max)))
  }

  test("toRanges yields all ranges") {
    check((ranges: Seq[Range]) => Diet(ranges).toRanges == ranges.map(r => r.min to r.max))
  }

  test("toNumericRanges yields all ranges") {
    check((ranges: Seq[Range]) => Diet(ranges).toNumericRanges == ranges.map(r => r.min to r.max))
  }

  test("toSeq matches all values sorted") {
    check((ranges: Seq[Range]) => Diet(ranges).toSeq == ranges.flatMap(_.toSet).toSet.toSeq.sorted)
  }

  test("toSet matches all values") {
    check((ranges: Seq[Range]) => Diet(ranges).toSet == ranges.flatMap(_.toSet).toSet)
  }

  test("min is smallest value") {
    check((ranges: Seq[Range]) => Diet(ranges).min == ranges.head.min)
  }

  test("minOption is Some(smallest value)") {
    check((ranges: Seq[Range]) => Diet(ranges).minOption == Some(ranges.head.min))
  }

  test("max is largest value") {
    check((ranges: Seq[Range]) => Diet(ranges).max == ranges.last.max)
  }

  test("maxOption is Some(largest value)") {
    check((ranges: Seq[Range]) => Diet(ranges).maxOption == Some(ranges.last.max))
  }

  test("toString formats all intervals") {
    check((ranges: Seq[Range]) =>
      Diet(ranges).toString == ranges
        .map(r => if r.size == 1 then r.min.toString else s"${r.min}..${r.max}")
        .mkString("Diet(", ", ", ")")
    )
  }

class DietCommandsTest extends UnitTest:
  class DietCommands(blockSize: Int, block: Gen[Int]) extends Commands:
    type State = Set[Int]
    final class Sut(var diet: Diet[Int])

    def first(b: Int) = (b.toLong * blockSize).toInt
    def last(b: Int) = ((b + 1L) * blockSize - 1).toInt
    def values(blocks: Range) =
      if blocks.isEmpty then blocks
      else if blocks.step > 0 then first(blocks.min) to last(blocks.max)
      else last(blocks.max) to first(blocks.min) by -1
    def runs(state: State): Seq[(Int, Int)] =
      state.toSeq.sorted
        .foldLeft(List.empty[(Int, Int)]) {
          case ((s, e) :: rest, b) if b == e + 1 => (s, b) :: rest
          case (acc, b) => (b, b) :: acc
        }
        .reverse
        .map((s, e) => (first(s), last(e)))

    val blocks =
      for
        from <- block
        length <- Gen.choose(0, 20)
        ascending = from to (from.toLong + length).min(Int.MaxValue / blockSize).toInt
        blocks <- Gen.oneOf(ascending, ascending.reverse, from until from)
      yield blocks
    val value = Gen.oneOf(block.map(first), block.map(last), block.flatMap(b => Gen.choose(first(b), last(b))))
    val valueRange =
      for
        a <- value
        b <- value
        range <- Gen.oneOf((a min b) to (a max b), (a max b) to (a min b) by -1, a until a)
      yield range

    def canCreateNewSut(newState: State, initSuts: Iterable[State], runningSuts: Iterable[Sut]) = true
    def newSut(state: State) = Sut(Diet(state))
    def destroySut(sut: Sut) = ()
    def initialPreCondition(state: State) = true
    def genInitialState = Gen.const(Set.empty[Int])
    def genCommand(state: State) =
      val commands = Seq(
        blocks.map(Add(_)),
        blocks.map(Remove(_)),
        Gen.listOf(blocks).map(Union(_)),
        Gen.listOf(blocks).map(Intersect(_)),
        Gen.listOf(blocks).map(Diff(_)),
        value.map(Contains(_)),
        valueRange.map(ContainsRange(_)),
        Gen.const(IsEmpty),
        Gen.const(NonEmpty),
        Gen.const(Min),
        Gen.const(MinOption),
        Gen.const(Max),
        Gen.const(MaxOption),
        Gen.const(IntervalsIterator),
        Gen.const(ToRanges),
        Gen.const(ToNumericRanges),
      )
      val unitOnly = Seq(block.map(AddValue(_)), block.map(RemoveValue(_)), Gen.const(Iterate), Gen.const(Size))
      Gen.oneOf(if blockSize == 1 then commands ++ unitOnly else commands).flatMap(identity)

    abstract class Update(f: Diet[Int] => Diet[Int], model: State => State) extends SuccessCommand:
      type Result = Diet[Int]
      def run(sut: Sut) =
        sut.diet = f(sut.diet)
        sut.diet
      def nextState(state: State) = model(state)
      def preCondition(state: State) = true
      def postCondition(state: State, diet: Diet[Int]): Prop =
        isCanonical(diet) && diet.toIntervals.map(i => (i.start, i.end)) == runs(model(state))

    case class AddValue(n: Int) extends Update(_ + n, _ + n)
    case class Add(blocks: Range) extends Update(_ + values(blocks), _ ++ blocks)
    case class RemoveValue(n: Int) extends Update(_ - n, _ - n)
    case class Remove(blocks: Range) extends Update(_ - values(blocks), _ -- blocks)
    case class Union(rhs: List[Range]) extends Update(_ ++ Diet(rhs.map(values)), _ ++ rhs.flatten)
    case class Intersect(rhs: List[Range]) extends Update(_ & Diet(rhs.map(values)), _ & rhs.flatten.toSet)
    case class Diff(rhs: List[Range]) extends Update(_ -- Diet(rhs.map(values)), _ -- rhs.flatten)

    abstract class Query[R](f: Diet[Int] => R, model: State => R) extends Command:
      type Result = R
      def run(sut: Sut) = f(sut.diet)
      def nextState(state: State) = state
      def preCondition(state: State) = true
      def postCondition(state: State, result: Try[R]): Prop = result.toOption == Try(model(state)).toOption

    case class Contains(n: Int) extends Query(_.contains(n), _.contains(Math.floorDiv(n, blockSize)))
    case class ContainsRange(range: Range)
        extends Query(
          _.contains(range),
          state =>
            range.isEmpty || (Math.floorDiv(range.min, blockSize) to Math.floorDiv(range.max, blockSize)).forall(state),
        )
    case object IsEmpty extends Query(_.isEmpty, _.isEmpty)
    case object NonEmpty extends Query(_.nonEmpty, _.nonEmpty)
    case object Min extends Query(_.min, state => first(state.min))
    case object MinOption extends Query(_.minOption, _.minOption.map(first))
    case object Max extends Query(_.max, state => last(state.max))
    case object MaxOption extends Query(_.maxOption, _.maxOption.map(last))
    case object IntervalsIterator extends Query(_.intervalsIterator.map(i => (i.start, i.end)).toSeq, runs)
    case object ToRanges extends Query(_.toRanges.map(r => (r.start, r.end)), runs)
    case object ToNumericRanges extends Query(_.toNumericRanges.map(r => (r.start, r.end)), runs)
    case object Iterate extends Query(_.iterator.take(1000).toSeq, _.toSeq.sorted.take(1000))
    case object Size extends Query(_.size, _.size)

  test("operations match a Set and keep the diet canonical") {
    val nearZeroAndTheLimits = Gen.oneOf(
      Gen.choose(-50, 50),
      Gen.choose(Int.MinValue, Int.MinValue + 50),
      Gen.choose(Int.MaxValue - 50, Int.MaxValue),
    )
    check(DietCommands(1, nearZeroAndTheLimits).property())
  }

  test("operations on huge diets match a Set of blocks and keep the diet canonical") {
    check(DietCommands(1 << 24, Gen.chooseNum(-128, 127)).property())
  }
