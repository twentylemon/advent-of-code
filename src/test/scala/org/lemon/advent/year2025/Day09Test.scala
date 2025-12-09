package org.lemon.advent.year2025

import org.lemon.advent.*
import org.lemon.advent.year2025.Day09.*

class Day09Test extends UnitTest:

  test("part 1 example") {
    val in = """|7,1
                |11,1
                |11,7
                |9,7
                |9,5
                |2,5
                |2,3
                |7,3
                |""".stripMargin
    part1(in) shouldBe 50
  }

  test("part 1") {
    part1(read(file(2025)(9))) shouldBe 4748985168L
  }

  test("part 2 example") {
    val in = """|7,1
                |11,1
                |11,7
                |9,7
                |9,5
                |2,5
                |2,3
                |7,3
                |""".stripMargin
    part2(in) shouldBe 24
  }

  test("part 2") {
    part2(read(file(2025)(9))) shouldBe 1550760868L
  }
