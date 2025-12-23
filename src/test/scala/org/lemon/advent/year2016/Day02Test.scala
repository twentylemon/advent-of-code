package org.lemon.advent.year2016

import org.lemon.advent.*
import org.lemon.advent.year2016.Day02.*

class Day02Test extends UnitTest:

  test("part 1 example") {
    val in = """|ULL
                |RRDDD
                |LURDL
                |UUUUD
                |""".stripMargin
    part1(in) shouldBe 1985
  }

  test("part 1") {
    part1(read(file(2016)(2))) shouldBe 35749
  }

  test("part 2 example") {
    val in = """|ULL
                |RRDDD
                |LURDL
                |UUUUD
                |""".stripMargin
    part2(in) shouldBe "5DB3"
  }

  test("part 2") {
    part2(read(file(2016)(2))) shouldBe "9365C"
  }
