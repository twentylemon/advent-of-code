package org.lemon.advent.year2015

import org.lemon.advent.*
import org.lemon.advent.year2015.Day01.*

class Day01Test extends UnitTest:

  for
    (input, expected) <- Seq(
      ("(())", 0),
      ("()()", 0),
      ("(((", 3),
      ("(()(()(", 3),
      ("))(((((", 3),
      (")))", -3),
      (")())())", -3),
    )
  do
    test(s"part 1 example $input") {
      part1(input) shouldBe expected
    }

  test("part 1") {
    part1(read(file(2015)(1))) shouldBe 138
  }

  for
    (input, expected) <- Seq(
      (")", 1),
      ("()())", 5),
    )
  do
    test(s"part 2 example $input") {
      part2(input) shouldBe expected
    }

  test("part 2") {
    part2(read(file(2015)(1))) shouldBe 1771
  }
