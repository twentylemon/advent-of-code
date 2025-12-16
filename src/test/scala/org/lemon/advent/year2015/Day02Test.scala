package org.lemon.advent.year2015

import org.lemon.advent.*
import org.lemon.advent.year2015.Day02.*

class Day02Test extends UnitTest:

  test("part 1 example") {
    val in = """|2x3x4
                |1x1x10
                |""".stripMargin
    part1(in) shouldBe 58 + 43
  }

  test("part 1") {
    part1(read(file(2015)(2))) shouldBe 1588178
  }

  test("part 2 example") {
    val in = """|2x3x4
                |1x1x10
                |""".stripMargin
    part2(in) shouldBe 34 + 14
  }

  test("part 2") {
    // part2(read(file(2015)(2))) shouldBe 3783758
  }
