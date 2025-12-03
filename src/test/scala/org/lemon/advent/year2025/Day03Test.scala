package org.lemon.advent.year2025

import org.lemon.advent._
import org.lemon.advent.year2025.Day03._

class Day03Test extends UnitTest:

  test("part 1 example") {
    val in = """|987654321111111
                |811111111111119
                |234234234234278
                |818181911112111
                |""".stripMargin
    part1(in) shouldBe 357
  }

  test("part 1") {
    part1(read(file(2025)(3))) shouldBe 17435
  }

  test("part 2 example") {
    val in = """|987654321111111
                |811111111111119
                |234234234234278
                |818181911112111
                |""".stripMargin
    part2(in) shouldBe 3121910778619L
  }

  test("part 2") {
    part2(read(file(2025)(3))) shouldBe 172886048065379L
  }
