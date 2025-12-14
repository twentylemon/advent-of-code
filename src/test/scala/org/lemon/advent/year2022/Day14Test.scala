package org.lemon.advent.year2022

import org.lemon.advent.*
import org.lemon.advent.year2022.Day14.*

class Day14Test extends UnitTest:

  test("part 1 example") {
    val in = """|498,4 -> 498,6 -> 496,6
                |503,4 -> 502,4 -> 502,9 -> 494,9""".stripMargin

    part1(in) shouldBe 24
  }

  test("part 1") {
    part1(read(file(2022)(14))) shouldBe 728
  }

  test("part 2 example") {
    val in = """|498,4 -> 498,6 -> 496,6
                |503,4 -> 502,4 -> 502,9 -> 494,9""".stripMargin

    part2(in) shouldBe 93
  }

  test("part 2") {
    part2(read(file(2022)(14))) shouldBe 27623
  }
