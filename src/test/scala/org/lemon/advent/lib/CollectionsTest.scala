package org.lemon.advent.lib

import org.scalacheck._
import org.scalacheck.Prop._
import org.lemon.advent._

class CollectionsTest extends UnitTest:

  test("split matches String#split") {
    given Arbitrary[(String, Char)] = Arbitrary(for
      delim <- Gen.alphaNumChar
      parts <- Gen.listOf(Gen.stringOf(Gen.alphaNumChar.suchThat(_ != delim)))
    yield (parts.mkString(delim.toString), delim))

    check((str: String, delim: Char) =>
      val expected = str.split(delim.toString, -1).filter(_.nonEmpty).toList
      val actual = str.toList.split(delim).map(_.mkString).toList
      actual == expected
    )
  }

  test("split on Vector preserves Vector type") {
    check((str: String, delim: Char) =>
      val result = str.toVector.split(delim)
      result.isInstanceOf[Vector[?]] && result.forall(_.isInstanceOf[Vector[?]])
    )
  }
