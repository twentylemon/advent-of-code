package org.lemon.advent.year2015

import org.lemon.advent.*
import org.lemon.advent.year2015.Day06.*

class Day06Test extends UnitTest:

  test("part 1 example") {
    val in = """|turn on 0,0 through 999,999
                |toggle 0,0 through 999,0
                |turn off 499,499 through 500,500
                |""".stripMargin
    part1(in) shouldBe 1000 * 1000 - 1000 - 4
  }

  test("part 1") {
    part1(read(file(2015)(6))) shouldBe 400410
  }

  for
    (input, expected) <- Seq(
      ("turn on 0,0 through 0,0", 1),
      ("toggle 0,0 through 999,999", 2000000)
    )
  do
    test(s"part 2 example $input") {
      part2(input) shouldBe expected
    }

  test("part 2") {
    part2(read(file(2015)(6))) shouldBe 15343601
  }
