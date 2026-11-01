package org.lemon.advent.lib

import org.lemon.advent.*
import org.lemon.advent.lib.lcm
import org.lemon.advent.lib.gcd
import org.scalacheck.{Arbitrary, Gen}
import org.scalacheck.Prop.*

class MathTest extends UnitTest:

  test("lcm of 1 and x is x") {
    check((x: Int) => x > 1 ==> (1.lcm(x) == x))
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
