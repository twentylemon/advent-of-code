package org.lemon.advent.year2022

import org.lemon.advent.*
import org.lemon.advent.year2022.Day24.*

class Day24Test extends UnitTest:

  test("part 1 example") {
    val in = """|#.######
                |#>>.<^<#
                |#.<..<<#
                |#>v.><>#
                |#<^v^^>#
                |######.#""".stripMargin

    part1(in) shouldBe 18
  }

  test("part 1") {
    part1(read(file(2022)(24))) shouldBe 247
  }

  test("part 2 example") {
    val in = """|#.######
                |#>>.<^<#
                |#.<..<<#
                |#>v.><>#
                |#<^v^^>#
                |######.#""".stripMargin

    part2(in) shouldBe 54
  }

  test("part 2") {
    part2(read(file(2022)(24))) shouldBe 728
  }
