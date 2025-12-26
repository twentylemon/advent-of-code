package org.lemon.advent.year2016

import org.lemon.advent.*
import org.lemon.advent.year2016.Day15.*

class Day15Test extends UnitTest:

  test("part 1 example") {
    val in = """|Disc #1 has 5 positions; at time=0, it is at position 4.
                |Disc #2 has 2 positions; at time=0, it is at position 1.
                |""".stripMargin
    part1(in) shouldBe 5
  }

  test("part 1") {
    part1(read(file(2016)(15))) shouldBe 122318
  }

  test("part 2") {
    part2(read(file(2016)(15))) shouldBe 3208583
  }
