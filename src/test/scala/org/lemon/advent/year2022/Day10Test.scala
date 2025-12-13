package org.lemon.advent.year2022

import org.lemon.advent.*
import org.lemon.advent.year2022.Day10.*

class Day10Test extends UnitTest:

  test("part 1 example") {
    part1(readLines("year2022/day10-test.txt")) shouldBe 13140
  }

  test("part 1") {
    part1(readLines(file(2022)(10))) shouldBe 17180
  }

  test("part 2 example") {
    part2(readLines("year2022/day10-test.txt")) shouldBe """|##..##..##..##..##..##..##..##..##..##..
                                                            |###...###...###...###...###...###...###.
                                                            |####....####....####....####....####....
                                                            |#####.....#####.....#####.....#####.....
                                                            |######......######......######......####
                                                            |#######.......#######.......#######.....""".stripMargin
  }

  test("part 2") {
    part2(readLines(file(2022)(10))) shouldBe """|###..####.#..#.###..###..#....#..#.###..
                                                 |#..#.#....#..#.#..#.#..#.#....#..#.#..#.
                                                 |#..#.###..####.#..#.#..#.#....#..#.###..
                                                 |###..#....#..#.###..###..#....#..#.#..#.
                                                 |#.#..#....#..#.#....#.#..#....#..#.#..#.
                                                 |#..#.####.#..#.#....#..#.####..##..###..""".stripMargin
  }
