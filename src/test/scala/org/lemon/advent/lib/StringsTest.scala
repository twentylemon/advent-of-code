package org.lemon.advent.lib

import org.lemon.advent.*
import org.scalacheck.Prop.*
import org.scalacheck.*
import org.scalacheck.Arbitrary.*

class StringsTest extends UnitTest:

  given Arbitrary[Seq[String]] = Arbitrary(Gen.nonEmptyListOf(Gen.alphaNumStr.suchThat(_.nonEmpty)))

  test("csv splits basic comma-separated string") {
    check((xs: Seq[String]) => xs.mkString(",").csv == xs)
  }

  test("csv splits with leading whitespace") {
    check((xs: Seq[String]) => xs.mkString(", ").csv == xs)
  }

  test("csv splits with trailing whitespace") {
    check((xs: Seq[String]) => xs.mkString(" ,").csv == xs)
  }

  test("csv filters out empty values") {
    "a,,b,c".csv == Seq("a", "b", "c")
  }

  test("csv handles empty string") {
    "".csv == Seq.empty
  }
