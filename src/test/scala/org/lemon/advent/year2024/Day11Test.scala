package org.lemon.advent.year2024

import org.lemon.advent.*
import org.lemon.advent.year2024.Day11.*

class Day11Test extends UnitTest:

  test("part 1 example") {
    val in = "125 17"
    part1(in) shouldBe 55312
  }

  test("part 1") {
    part1(read(file(2024)(11))) shouldBe 193607
  }

  test("part 2") {
    part2(read(file(2024)(11))) shouldBe 229557103025807L
  }
