package org.lemon.advent.year2022

import org.lemon.advent.*
import org.lemon.advent.year2022.Day12.*

class Day12Test extends UnitTest:

  test("part 1 example") {
    val in = """|Sabqponm
                |abcryxxl
                |accszExk
                |acctuvwj
                |abdefghi""".stripMargin

    part1(in.linesIterator.toSeq) shouldBe 31
  }

  test("part 1") {
    part1(readLines(file(2022)(12))) shouldBe 534
  }

  test("part 2 example") {
    val in = """|Sabqponm
                |abcryxxl
                |accszExk
                |acctuvwj
                |abdefghi""".stripMargin

    part2(in.linesIterator.toSeq) shouldBe 29
  }

  test("part 2") {
    part2(readLines(file(2022)(12))) shouldBe 525
  }
