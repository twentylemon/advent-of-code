package org.lemon.advent.year2023

import org.lemon.advent.*
import org.lemon.advent.year2023.Day14.*

class Day14Test extends UnitTest:

  val in = """|O....#....
              |O.OO#....#
              |.....##...
              |OO.#O....O
              |.O.....O#.
              |O.#..O.#.#
              |..O..#O..O
              |.......O..
              |#....###..
              |#OO..#....""".stripMargin

  test("part 1 example") {
    part1(in) shouldBe 136
  }

  test("part 1") {
    part1(read(file(2023)(14))) shouldBe 106517
  }

  test("part 2 example") {
    part2(in) shouldBe 64
  }

  test("part 2") {
    part2(read(file(2023)(14))) shouldBe 79723
  }
