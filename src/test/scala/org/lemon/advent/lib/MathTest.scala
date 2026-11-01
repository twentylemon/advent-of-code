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

  Seq(2, 3, 5, 7, 11, 17).pairs.foreach((a, b) =>
    test(s"lcm of coprime numbers $a and $b = $a*$b") {
      a.lcm(b) == a * b
    }
  )

  (2 to 10 by 2).pairs.foreach((a, b) =>
    test(s"lcm of multiples of 2 numbers $a and $b = 2") {
      a.lcm(b) == 2
    }
  )
