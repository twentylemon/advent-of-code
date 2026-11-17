package org.lemon.advent.lib

import org.lemon.advent.*
import org.scalacheck.Gen
import org.scalacheck.Arbitrary.*
import org.scalacheck.Prop.*

class MathTest extends UnitTest:

  test("+% is the non-negative remainder for an Int modulus") {
    check((a: Int, n: Int) => n != 0 ==> (a +% n == Math.floorMod(a.toLong, n.toLong.abs).toInt))
  }

  test("+% is the non-negative remainder for a Long modulus") {
    check((a: Long, n: Long) => n != 0 ==> (a +% n == BigInt(a).mod(BigInt(n).abs).toLong))
  }

  test("/^ is ceiling division for Int") {
    check((a: Int, n: Int) => n != 0 ==> (a /^ n == (-Math.floorDiv(-a.toLong, n.toLong)).toInt))
  }

  test("/^ is ceiling division for Long") {
    check((a: Long, n: Long) =>
      n != 0 ==> (a /^ n == (BigDecimal(a) / BigDecimal(n)).setScale(0, BigDecimal.RoundingMode.CEILING).toLong)
    )
  }

  test("gcd matches BigInt gcd for Int") {
    check((a: Int, b: Int) => a.gcd(b) == BigInt(a).gcd(BigInt(b)).toInt)
  }

  test("gcd matches BigInt gcd for Long") {
    check((a: Long, b: Long) => a.gcd(b) == BigInt(a).gcd(BigInt(b)).toLong)
  }

  test("gcd of an iterable matches BigInt gcd") {
    check(forAll(Gen.nonEmptyListOf(arbitrary[Int])) { xs => xs.gcd == xs.map(BigInt(_)).reduce(_ `gcd` _).toInt })
  }

  val commonFactorPairs =
    for
      x <- arbitrary[Int]
      y <- arbitrary[Int]
      common <- Gen.chooseNum(1L, Long.MaxValue / (x.toLong.abs * y.toLong.abs).max(1))
    yield (common * x, common * y)

  test("lcm with 0 is 0") {
    check((a: Long) => a.lcm(0L) == 0 && 0L.lcm(a) == 0)
  }

  test("lcm is non-negative") {
    check(forAll(commonFactorPairs) { (a, b) => a.lcm(b) >= 0 })
  }

  test("lcm is a multiple of both arguments") {
    check(forAll(commonFactorPairs) { (a, b) => (a != 0 && b != 0) ==> (a.lcm(b) % a == 0 && a.lcm(b) % b == 0) })
  }

  test("lcm divided by each argument leaves coprime quotients") {
    check(forAll(commonFactorPairs) { (a, b) =>
      (a != 0 && b != 0) ==> {
        val lcm = a.lcm(b)
        (lcm / a).gcd(lcm / b) == 1
      }
    })
  }

  test("lcm of an iterable is the least common multiple") {
    check(forAll(Gen.nonEmptyListOf(arbitrary[Int].suchThat(_ != 0).map(BigInt(_)))) { xs =>
      val lcm = xs.lcm
      xs.forall(lcm % _ == 0) && xs.map(lcm / _).gcd == 1
    })
  }

  test("lcm of an iterable containing 0 is 0") {
    check((xs: List[Int], ys: List[Int]) => (xs ++ (0 :: ys)).lcm == 0)
  }
