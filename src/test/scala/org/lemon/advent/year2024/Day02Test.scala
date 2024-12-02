package org.lemon.advent.year2024

import org.lemon.advent._
import org.lemon.advent.year2024.Day02._

class Day02Test extends UnitTest:

  test("part 1 example") {
    val in = """|7 6 4 2 1
                |1 2 7 8 9
                |9 7 6 2 1
                |1 3 2 4 5
                |8 6 4 4 1
                |1 3 6 7 9""".stripMargin
    part1(in) shouldBe 2
  }

  test("part 1") {
    part1(read(file(2024)(2))) shouldBe 631
  }

  test("part 2 example") {
    val in = """|7 6 4 2 1
                |1 2 7 8 9
                |9 7 6 2 1
                |1 3 2 4 5
                |8 6 4 4 1
                |1 3 6 7 9""".stripMargin
    part2(in) shouldBe 4
  }

  test("part 2") {
    part2(read(file(2024)(2))) shouldBe 665
  }
