package org.lemon.advent.year2016

import org.lemon.advent.*
import org.lemon.advent.year2016.Day23.*

class Day23Test extends UnitTest:

  test("part 1 example") {
    val in = """|cpy 2 a
                |tgl a
                |tgl a
                |tgl a
                |cpy 1 a
                |dec a
                |dec a
                |""".stripMargin
    part1(in) shouldBe 3
  }

  test("part 1") {
    part1(read(file(2016)(23))) shouldBe 11739
  }

  test("part 2") {
    part2(read(file(2016)(23))) shouldBe 479008299
  }
