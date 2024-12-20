package org.lemon.advent.year2024

import org.lemon.advent._
import org.lemon.advent.year2024.Day20._

class Day20Test extends UnitTest:

  test("part 1 example") {
    val in = """|###############
                |#...#...#.....#
                |#.#.#.#.#.###.#
                |#S#...#.#.#...#
                |#######.#.#.###
                |#######.#.#...#
                |#######.#.###.#
                |###..E#...#...#
                |###.#######.###
                |#...###...#...#
                |#.#####.#.###.#
                |#.#...#.#.#...#
                |#.#.#.#.#.#.###
                |#...#...#...###
                |###############""".stripMargin
    part1(in, minSaving = 10) shouldBe 10
  }

  test("part 1") {
    part1(read(file(2024)(20))) shouldBe 1289
  }

  test("part 2 example") {
    val in = """|###############
                |#...#...#.....#
                |#.#.#.#.#.###.#
                |#S#...#.#.#...#
                |#######.#.#.###
                |#######.#.#...#
                |#######.#.###.#
                |###..E#...#...#
                |###.#######.###
                |#...###...#...#
                |#.#####.#.###.#
                |#.#...#.#.#...#
                |#.#.#.#.#.#.###
                |#...#...#...###
                |###############""".stripMargin
    part2(in, minSaving = 50) shouldBe 285
  }

  test("part 2") {
    part2(read(file(2024)(20))) shouldBe 982425
  }
