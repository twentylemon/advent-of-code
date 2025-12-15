package org.lemon.advent.lib

import org.scalacheck.*
import org.scalacheck.Prop.*
import org.lemon.advent.*

class CollectionsTest extends UnitTest:

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

  test("frequencies counts match elements") {
    check(forAll { (it: Iterable[Int]) =>
      val freq = it.frequencies
      it.forall(elem => freq(elem) == it.count(_ == elem))
    })
  }

  test("frequencies keys are exactly the distinct elements") {
    check(forAll { (it: Iterable[Int]) => it.frequencies.keySet == it.toSet })
  }
