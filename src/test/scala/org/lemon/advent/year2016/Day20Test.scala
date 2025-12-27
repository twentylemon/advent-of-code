package org.lemon.advent.year2016

import org.lemon.advent.*
import org.lemon.advent.year2016.Day20.*

class Day20Test extends UnitTest:

  test("part 1 example") {
    val in = """|5-8
                |0-2
                |4-7
                |""".stripMargin
    part1(in) shouldBe 3
  }

  test("part 1") {
    part1(read(file(2016)(20))) shouldBe 32259706
  }

  test("part 2 example") {
    val in = """|5-8
                |0-2
                |4-7
                |""".stripMargin
    part2(in, total = (0 to 9).size) shouldBe 2
  }

  test("part 2") {
    part2(read(file(2016)(20))) shouldBe 113
  }
