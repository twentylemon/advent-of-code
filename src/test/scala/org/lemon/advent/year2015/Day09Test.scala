package org.lemon.advent.year2015

import org.lemon.advent.*
import org.lemon.advent.year2015.Day09.*

class Day09Test extends UnitTest:

  test("part 1 example") {
    val in = """|London to Dublin = 464
                |London to Belfast = 518
                |Dublin to Belfast = 141
                |""".stripMargin
    part1(in) shouldBe 605
  }

  test("part 1") {
    part1(read(file(2015)(9))) shouldBe 117
  }

  test("part 2 example") {
    val in = """|London to Dublin = 464
                |London to Belfast = 518
                |Dublin to Belfast = 141
                |""".stripMargin
    part2(in) shouldBe 982
  }

  test("part 2") {
    part2(read(file(2015)(9))) shouldBe 909
  }
