package org.lemon.advent.year2024

import org.lemon.advent.*
import org.lemon.advent.year2024.Day09.*

class Day09Test extends UnitTest:

  test("part 1 example") {
    val in = "2333133121414131402"
    part1(in) shouldBe 1928
  }

  test("part 1") {
    part1(read(file(2024)(9))) shouldBe 6344673854800L
  }

  test("part 2 example") {
    val in = "2333133121414131402"
    part2(in) shouldBe 2858
  }

  test("part 2") {
    part2(read(file(2024)(9))) shouldBe 6360363199987L
  }
