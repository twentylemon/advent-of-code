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

  test("wsv splits basic space-separated string") {
    check((xs: Seq[String]) => xs.mkString(" ").wsv == xs)
  }

  test("wsv splits with multiple spaces") {
    check((xs: Seq[String]) => xs.mkString("  ").wsv == xs)
  }

  test("wsv splits with any whitespace") {
    val whitespace = Gen.nonEmptyListOf(Gen.oneOf(' ', '\t', '\n')).map(_.mkString)
    check(forAll(arbitrary[Seq[String]], whitespace) { (xs, ws) => xs.mkString(ws).wsv == xs })
  }

  test("wsv handles empty string") {
    "".wsv == Seq.empty
  }

  test("chunks splits basic double-newline-separated string") {
    check((xs: Seq[String]) => xs.mkString("\n\n").chunks == xs)
  }

  test("chunks splits with multiple blank lines") {
    check((xs: Seq[String]) => xs.mkString("\n\n\n").chunks == xs)
  }

  test("chunks handles empty string") {
    "".chunks == Seq.empty
  }
