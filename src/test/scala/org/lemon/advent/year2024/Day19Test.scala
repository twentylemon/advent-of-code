package org.lemon.advent.year2024

import org.lemon.advent.*
import org.lemon.advent.year2024.Day19.*

class Day19Test extends UnitTest:

  test("part 1 example") {
    val in = """|r, wr, b, g, bwu, rb, gb, br
                |
                |brwrr
                |bggr
                |gbbr
                |rrbgbr
                |ubwu
                |bwurrg
                |brgr
                |bbrgwb""".stripMargin
    part1(in) shouldBe 6
  }

  test("part 1") {
    part1(read(file(2024)(19))) shouldBe 311
  }

  test("part 2 example") {
    val in = """|r, wr, b, g, bwu, rb, gb, br
                |
                |brwrr
                |bggr
                |gbbr
                |rrbgbr
                |ubwu
                |bwurrg
                |brgr
                |bbrgwb""".stripMargin
    part2(in) shouldBe 16
  }

  test("part 2") {
    part2(read(file(2024)(19))) shouldBe 616234236468263L
  }
