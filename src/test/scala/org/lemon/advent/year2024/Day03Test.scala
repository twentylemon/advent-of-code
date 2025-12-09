package org.lemon.advent.year2024

import org.lemon.advent.*
import org.lemon.advent.year2024.Day03.*

class Day03Test extends UnitTest:

  test("part 1 example") {
    val in = "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))"
    part1(in) shouldBe 161
  }

  test("part 1") {
    part1(read(file(2024)(3))) shouldBe 167650499
  }

  test("part 2 example") {
    val in = "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))"
    part2(in) shouldBe 48
  }

  test("part 2") {
    part2(read(file(2024)(3))) shouldBe 95846796
  }
