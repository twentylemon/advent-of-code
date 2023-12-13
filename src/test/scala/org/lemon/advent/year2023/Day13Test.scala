package org.lemon.advent.year2023

import org.lemon.advent._
import org.lemon.advent.year2023.Day13._

class Day13Test extends UnitTest:

  val in = """|#.##..##.
              |..#.##.#.
              |##......#
              |##......#
              |..#.##.#.
              |..##..##.
              |#.#.##.#.
              |
              |#...##..#
              |#....#..#
              |..##..###
              |#####.##.
              |#####.##.
              |..##..###
              |#....#..#""".stripMargin

  test("part 1 example") {
    part1(in) shouldBe 405
  }

  test("part 1") {
    part1(read(file(2023)(13))) shouldBe 32371
  }

  test("part 2 example") {
    part2(in) shouldBe 400
  }

  test("part 2") {
    part2(read(file(2023)(13))) shouldBe 37416
  }
