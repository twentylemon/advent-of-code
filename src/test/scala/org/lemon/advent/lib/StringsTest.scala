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

  test("chunks handles multiple blank lines") {
    // multiple consecutive newlines should produce the same chunks when trimmed
    given Arbitrary[Int] = Arbitrary(Gen.choose(3, 10))
    check((xs: Seq[String], gap: Int) =>
      xs.mkString("\n\n").chunks.map(_.trim) == xs.mkString("\n" * gap).chunks.map(_.trim)
    )
  }

  test("chunks handles empty string") {
    "".chunks == Seq.empty
  }

  test("toBigInt round trip") {
    check((n: BigInt) => n.toString.toBigInt == n)
  }

  test("toBigInt works with + prefix") {
    check((n: BigInt) => ("+" + n.abs.toString).toBigInt == n.abs)
  }

  test("toBigInt works with - prefix") {
    check((n: BigInt) => ("-" + n.abs.toString).toBigInt == -n.abs)
  }

  test("toBigIntOption round trip") {
    check((n: BigInt) => n.toString.toBigIntOption == Some(n))
  }

  test("toBigIntOption returns None for invalid strings") {
    check(forAll(Gen.alphaStr.suchThat(_.nonEmpty))(s => s.toBigIntOption == None))
  }
