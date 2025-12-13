package org.lemon.advent.year2022

import org.lemon.advent.*
import org.lemon.advent.year2022.Day25.*

class Day25Test extends UnitTest:

  import Snafus.*

  for
    (lhs, rhs, expected) <- Seq[(Snafu, Snafu, Snafu)](
      ("0", "0", "0"), // 0 + 0 = 0
      ("1", "1", "2"), // 1 + 1 = 2
      ("2", "2", "1-"), // 2 + 2 = 4
      ("-", "-", "="), // -1 + -1 = -2
      ("=", "=", "-1"), // -2 + -2 = -4
      ("=", "==", "-21"), // -2 + -12 = -14
      ("2", "22", "1=-"), // 2 + 12 = 14
      ("1=", "1=", "11"), // 3 + 3 = 6
      ("1=", "=", "1"), // 3 + -2 = 1
      ("2-", "2=", "1=2"), // 9 + 8 = 17
      ("22", "22", "10-"), // 12 + 12 = 24
      ("10", "1", "11"), // 5 + 1 = 6
      ("1-", "=", "2"), // 4 + -2 = 2
      ("2=", "2=", "1=1"), // 8 + 8 = 16
      ("1==", "1==", "101"), // 13 + 13 = 26
    )
  do
    test(s"snafu `$lhs + $rhs = $expected") {
      (lhs + rhs) shouldBe expected
    }

  test("part 1 example") {
    val in = """|1=-0-2
                |12111
                |2=0=
                |21
                |2=01
                |111
                |20012
                |112
                |1=-1=
                |1-12
                |12
                |1=
                |122""".stripMargin

    part1(in.linesIterator.toSeq) shouldBe Snafu("2=-1=0")
  }

  test("part 1") {
    part1(readLines(file(2022)(25))) shouldBe Snafu("2=0=02-0----2-=02-10")
  }
