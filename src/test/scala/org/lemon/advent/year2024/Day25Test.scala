package org.lemon.advent.year2024

import org.lemon.advent._
import org.lemon.advent.year2024.Day25._

class Day25Test extends UnitTest:

  test("part 1 example") {
    val in = """|#####
                |.####
                |.####
                |.####
                |.#.#.
                |.#...
                |.....
                |
                |#####
                |##.##
                |.#.##
                |...##
                |...#.
                |...#.
                |.....
                |
                |.....
                |#....
                |#....
                |#...#
                |#.#.#
                |#.###
                |#####
                |
                |.....
                |.....
                |#.#..
                |###..
                |###.#
                |###.#
                |#####
                |
                |.....
                |.....
                |.....
                |#....
                |#.#..
                |#.#.#
                |#####""".stripMargin
    part1(in) shouldBe 3
  }

  test("part 1") {
    part1(read(file(2024)(25))) shouldBe 2900
  }
