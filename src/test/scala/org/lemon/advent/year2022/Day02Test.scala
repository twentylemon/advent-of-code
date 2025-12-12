package org.lemon.advent.year2022

import org.lemon.advent.*
import org.lemon.advent.year2022.Day02.*

class Day02Test extends UnitTest:

  test("part 1 example") {
    val in = """|A Y
                |B X
                |C Z""".stripMargin
    part1(in) shouldBe 15
  }

  test("part 1") {
    part1(read(file(2022)(2))) shouldBe 10718
  }

  test("part 2 example") {
    val in = """|A Y
                |B X
                |C Z""".stripMargin
    part2(in) shouldBe 12
  }

  test("part 2") {
    part2(read(file(2022)(2))) shouldBe 14652
  }
