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
      val splitResult = str.toList.split(delim)
      val actual = splitResult.map(_.mkString).toList
      actual == expected
    })
  }
