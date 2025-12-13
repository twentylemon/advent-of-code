package org.lemon.advent.year2022

import org.lemon.advent.*
import org.lemon.advent.year2022.Day08.*

class Day08Test extends UnitTest:

  test("part 1 example") {
    val in = """|30373
                |25512
                |65332
                |33549
                |35390""".stripMargin

    part1(in) shouldBe 21
  }

  test("part 1") {
    part1(read(file(2022)(8))) shouldBe 1538
  }

  test("part 2 example") {
    val in = """|30373
                |25512
                |65332
                |33549
                |35390""".stripMargin

    part2(in) shouldBe 8
  }

  test("part 2") {
    part2(read(file(2022)(8))) shouldBe 496125
  }
