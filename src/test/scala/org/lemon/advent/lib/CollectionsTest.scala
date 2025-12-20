package org.lemon.advent.lib

import org.scalacheck.*
import org.scalacheck.Prop.*
import org.lemon.advent.*

class CollectionsTest extends UnitTest:

  test("pairs is equal to stdlib combinations(2) of a set") {
    check((xs: Set[Int]) =>
      val pair = xs.pairs.toSeq.sorted
      val combo = xs.toSeq
        .combinations(2)
        .map { case Seq(a, b) => (a, b) }
        .toSeq
        .sorted
      pair == combo
    )
  }

  test("triples is equal to stdlib combinations(3) of a set") {
    check((xs: Set[Int]) =>
      val trip = xs.triples.toSeq.sorted
      val combo = xs.toSeq
        .combinations(3)
        .map { case Seq(a, b, c) => (a, b, c) }
        .toSeq
        .sorted
      trip == combo
    )
  }

  test("sliding2 is equivalent to sliding(2) with singletons removed") {
    check((xs: Seq[Int]) => xs.sliding(2).collect { case Seq(a, b) => (a, b) }.toSeq == xs.sliding2.toSeq)
  }

  test("iterator sliding2 is equivalent to sliding(2) with singletons removed") {
    check((xs: Seq[Int]) =>
      xs.iterator.sliding(2).collect { case Seq(a, b) => (a, b) }.toSeq == xs.iterator.sliding2.toSeq
    )
  }

  test("sliding3 is equivalent to sliding(3) with partial windows removed") {
    check((xs: Seq[Int]) =>
      xs.sliding(3).collect { case Seq(a, b, c) => (a, b, c) }.toSeq == xs.sliding3.toSeq
    )
  }

  test("iterator sliding3 is equivalent to sliding(3) with partial windows removed") {
    check((xs: Seq[Int]) =>
      xs.iterator.sliding(3).collect { case Seq(a, b, c) => (a, b, c) }.toSeq == xs.iterator.sliding3.toSeq
    )
  }

  test("sliding4 is equivalent to sliding(4) with partial windows removed") {
    check((xs: Seq[Int]) =>
      xs.sliding(4).collect { case Seq(a, b, c, d) => (a, b, c, d) }.toSeq == xs.sliding4.toSeq
    )
  }

  test("iterator sliding4 is equivalent to sliding(4) with partial windows removed") {
    check((xs: Seq[Int]) =>
      xs.iterator.sliding(4).collect { case Seq(a, b, c, d) => (a, b, c, d) }.toSeq == xs.iterator.sliding4.toSeq
    )
  }

  test("frequencies counts match elements") {
    check(forAll { (it: Iterable[Int]) =>
      val freq = it.frequencies
      it.forall(elem => freq(elem) == it.count(_ == elem))
    })
  }

  test("frequencies keys are exactly the distinct elements") {
    check(forAll { (it: Iterable[Int]) => it.frequencies.keySet == it.toSet })
  }

  test("split matches String#split") {
    val gen =
      for
        delim <- Gen.alphaNumChar
        repeat <- Gen.choose(1, 10)
        parts <- Gen.listOf(Gen.stringOf(Gen.alphaNumChar.suchThat(_ != delim)))
      yield (parts.mkString(delim.toString.repeat(repeat)), delim)

    check(forAll(gen) { (str: String, delim: Char) =>
      val expected = str.split(delim.toString).toList
      val splitResult = str.toList.map(_.toString).split(delim.toString)
      val actual = splitResult.map(_.mkString).toList
      actual == expected
    })
  }
