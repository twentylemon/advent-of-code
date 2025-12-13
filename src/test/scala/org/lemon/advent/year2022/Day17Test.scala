package org.lemon.advent.year2022

import org.lemon.advent.*
import org.lemon.advent.year2022.Day17.*

class Day17Test extends UnitTest:

  test("part 1 example") {
    val in = ">>><<><>><<<>><>>><<<>>><<<><<<>><>><<>>"
    part1(in) shouldBe 3068
  }

  test("part 1") {
    part1(read(file(2022)(17))) shouldBe 3232
  }

  test("part 2 example") {
    val in = ">>><<><>><<<>><>>><<<>>><<<><<<>><>><<>>"
    part2(in) shouldBe 1514285714288L
  }

  test("part 2") {
    part2(read(file(2022)(17))) shouldBe 1585632183915L
  }
