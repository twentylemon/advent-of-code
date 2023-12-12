package org.lemon.advent.lib

import org.lemon.advent._
import org.lemon.advent.lib.lcm
import org.lemon.advent.lib.gcd
import org.scalacheck.Prop._

class MathTest extends UnitTest:

  test("lcm of 1 and x is x") {
    check((x: Int) => x > 1 ==> (lcm(1, x) == x))
  }

  Seq(2, 3, 5, 7, 11, 17).pairs.foreach((a, b) => 
      test(s"lcm of coprime numbers $a and $b = $a*$b") {
        lcm(a, b) == a * b
      }
    )

  (2 to 10 by 2).pairs.foreach((a, b) => 
      test(s"lcm of multiples of 2 numbers $a and $b = 2") {
        lcm(a, b) == 2
      }
    )
