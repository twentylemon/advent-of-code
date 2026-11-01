package org.lemon.advent.lib

import org.lemon.advent.*
import org.lemon.advent.lib.lcm
import org.lemon.advent.lib.gcd
import org.scalacheck.{Arbitrary, Gen}
import org.scalacheck.Prop.*

class MathTest extends UnitTest:

  test("/^ is ceiling division") {
    check(forAll(Arbitrary.arbitrary[Int], Gen.choose(1, 1000)) { (a: Int, n: Int) =>
      n > 0 ==> (a /^ n == -Math.floorDiv(-a.toLong, n.toLong))
    })
  }

  test("lcm of 1 and x is x") {
    check((x: Int) => x > 1 ==> (1.lcm(x) == x))
  }

  test("lcm matches BigInt when the result fits in a Long") {
    // share a big common factor so a*b overflows even though lcm(a, b) does not
    val gen =
      for
        common <- Gen.choose(1L, 1L << 40)
        x <- Gen.choose(1L, 1000L)
        y <- Gen.choose(1L, 1000L)
      yield (common * x, common * y)

    check(forAll(gen) { (a: Long, b: Long) =>
      val expected = BigInt(a) * BigInt(b) / BigInt(a).gcd(BigInt(b))
      (a > 0 && b > 0 && expected.isValidLong) ==> (a.lcm(b) == expected.toLong)
    })
  }

  test("gcd matches BigInt gcd") {
    check((a: Int, b: Int) => (a != Int.MinValue && b != Int.MinValue) ==> (a.gcd(b) == BigInt(a).gcd(BigInt(b))))
  }

  Seq(2, 3, 5, 7, 11, 17).pairs.foreach((a, b) =>
    test(s"lcm of coprime numbers $a and $b = $a*$b") {
      a.lcm(b) shouldBe a * b
    }
  )

  (2 to 10 by 2).pairs.foreach((a, b) =>
    test(s"gcd of multiples of 2 numbers $a and $b is a multiple of 2") {
      a.gcd(b) % 2 shouldBe 0
    }
  )

  test("lcm is the smallest positive common multiple") {
    given Arbitrary[Int] = Arbitrary(Gen.choose(1, 1000))
    check((a: Int, b: Int) =>
      val lcm = a.lcm(b)
      lcm % a == 0 && lcm % b == 0 && (1 until lcm).forall(m => m % a != 0 || m % b != 0)
    )
  }
