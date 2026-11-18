package org.lemon.advent.lib

import org.lemon.advent.*
import org.scalacheck.Prop.*
import org.scalacheck.*
import org.scalacheck.Arbitrary.*

class StringsTest extends UnitTest:

  val csvField = Gen.asciiPrintableStr.map(_.filterNot(_ == ',').trim).suchThat(_.nonEmpty)
  val csvFields = Gen.nonEmptyListOf(csvField)
  val wsvTokens = Gen.nonEmptyListOf(Gen.asciiPrintableStr.map(_.filterNot(_.isWhitespace)).suchThat(_.nonEmpty))
  val chunkLine = Gen.asciiPrintableStr.suchThat(_.trim.nonEmpty)
  val chunkTexts = Gen.nonEmptyListOf(Gen.nonEmptyListOf(chunkLine).map(_.mkString("\n")))

  test("padLeft leaves a string at least len long unchanged") {
    val gen =
      for
        str <- arbitrary[String]
        len <- Gen.chooseNum(Int.MinValue, str.length)
      yield (str, len)
    check(forAllNoShrink(gen, arbitrary[Char]) { case ((str, len), c) => str.padLeft(len, c) == str })
  }

  test("padLeft matches padTo on the reversed string") {
    val gen =
      for
        str <- arbitrary[String]
        pad <- Gen.sized(n => Gen.choose(0, n))
      yield (str, str.length + pad)
    check(forAll(gen, arbitrary[Char]) { case ((str, len), c) =>
      str.padLeft(len, c) == str.reverse.padTo(len, c).reverse
    })
  }

  test("csv splits basic comma-separated string") {
    check(forAll(csvFields)(xs => xs.mkString(",").csv == xs))
  }

  test("csv splits with leading whitespace") {
    check(forAll(csvFields)(xs => xs.mkString(", ").csv == xs))
  }

  test("csv splits with trailing whitespace") {
    check(forAll(csvFields)(xs => xs.mkString(" ,").csv == xs))
  }

  test("csv drops empty and blank values") {
    val blank = Gen.stringOf(Gen.oneOf(' ', '\t'))
    val parts = Gen.listOf(Gen.either(blank, csvField))
    check(forAll(parts)(parts => parts.map(_.merge).mkString(",").csv == parts.collect { case Right(x) => x }))
  }

  test("csv handles empty string") {
    "".csv shouldBe empty
  }

  test("wsv splits basic space-separated string") {
    check(forAll(wsvTokens)(xs => xs.mkString(" ").wsv == xs))
  }

  test("wsv splits with multiple spaces") {
    check(forAll(wsvTokens)(xs => xs.mkString("  ").wsv == xs))
  }

  test("wsv splits with any whitespace") {
    val whitespace = Gen.nonEmptyListOf(Gen.oneOf(' ', '\t', '\r', '\n')).map(_.mkString)
    check(forAll(wsvTokens, whitespace) { (xs, ws) => xs.mkString(ws).wsv == xs })
  }

  test("wsv handles empty string") {
    "".wsv shouldBe empty
  }

  test("chunks splits basic double-newline-separated string") {
    check(forAll(chunkTexts)(xs => xs.mkString("\n\n").chunks == xs))
  }

  test("chunks handles multiple blank lines") {
    // multiple consecutive newlines should produce the same chunks when trimmed
    given Arbitrary[Int] = Arbitrary(Gen.choose(3, 10))
    check(forAll(chunkTexts, arbitrary[Int]) { (xs, gap) =>
      xs.mkString("\n\n").chunks.map(_.trim) == xs.mkString("\n" * gap).chunks.map(_.trim)
    })
  }

  test("chunks handles empty string") {
    "".chunks shouldBe empty
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

  test("toBigInt ignores surrounding whitespace") {
    val whitespace = Gen.listOf(Gen.oneOf(' ', '\t', '\n')).map(_.mkString)
    check(forAll(arbitrary[BigInt], whitespace, whitespace) { (n, pre, post) => s"$pre$n$post".toBigInt == n })
  }

  test("toBigIntOption round trip") {
    check((n: BigInt) => n.toString.toBigIntOption == Some(n))
  }

  test("toBigIntOption ignores surrounding whitespace") {
    val whitespace = Gen.listOf(Gen.oneOf(' ', '\t', '\n')).map(_.mkString)
    check(forAll(arbitrary[BigInt], whitespace, whitespace) { (n, pre, post) =>
      s"$pre$n$post".toBigIntOption == Some(n)
    })
  }

  test("toBigIntOption returns None for invalid strings") {
    check(forAll(Gen.alphaStr.suchThat(_.nonEmpty))(s => s.toBigIntOption == None))
  }
