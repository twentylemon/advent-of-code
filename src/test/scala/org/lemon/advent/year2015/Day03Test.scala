package org.lemon.advent.year2015

import org.lemon.advent.*
import org.lemon.advent.year2015.Day03.*

class Day03Test extends UnitTest:

  for (input, expected) <- Seq(
    (">", 2),
    ("^>v<", 4),
    ("^v^v^v^v^v", 2),
  ) do
    test(s"part 1 example $input") {
      part1(input) shouldBe expected
    }

  test("part 1") {
    part1(read(file(2015)(3))) shouldBe 2572
  }

  for (input, expected) <- Seq(
    ("^v", 3),
    ("^>v<", 3),
    ("^v^v^v^v^v", 11),
  ) do
    test(s"part 2 example $input") {
      part2(input) shouldBe expected
    }

  test("part 2") {
    part2(read(file(2015)(3))) shouldBe 2631
  }
