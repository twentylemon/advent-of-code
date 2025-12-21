package org.lemon.advent.year2015

import org.lemon.advent.*
import org.lemon.advent.year2015.Day18.*

class Day18Test extends UnitTest:

  test("part 1 example") {
    val in = """|.#.#.#
                |...##.
                |#....#
                |..#...
                |#.#..#
                |####..
                |""".stripMargin
    part1(in, steps = 4) shouldBe 4
  }

  test("part 1") {
    part1(read(file(2015)(18))) shouldBe 1061
  }

  test("part 2 example") {
    val in = """|.#.#.#
                |...##.
                |#....#
                |..#...
                |#.#..#
                |####..
                |""".stripMargin
    part2(in, steps = 5) shouldBe 17
  }

  test("part 2") {
    part2(read(file(2015)(18))) shouldBe 0
  }
