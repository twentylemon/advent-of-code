package org.lemon.advent.year2022

import org.lemon.advent.*
import org.lemon.advent.year2022.Day01.*

class Day01Test extends UnitTest:

  test("part 1 example") {
    val in = """|1000
                |2000
                |3000
                |
                |4000
                |
                |5000
                |6000
                |
                |7000
                |8000
                |9000
                |
                |10000""".stripMargin
    part1(in) shouldBe 24000
  }

  test("part 1") {
    part1(read(file(2022)(1))) shouldBe 69795
  }

  test("part 2") {
    part2(read(file(2022)(1))) shouldBe 208437
  }
