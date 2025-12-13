package org.lemon.advent.year2022

import org.lemon.advent.*
import org.lemon.advent.year2022.Day09.*

class Day09Test extends UnitTest:

  test("part 1 example") {
    val in = """|R 4
                |U 4
                |L 3
                |D 1
                |R 4
                |D 1
                |L 5
                |R 2""".stripMargin

    part1(in) shouldBe 13
  }

  test("part 1") {
    part1(read(file(2022)(9))) shouldBe 6503
  }

  test("part 2 example 1") {
    val in = """|R 4
                |U 4
                |L 3
                |D 1
                |R 4
                |D 1
                |L 5
                |R 2""".stripMargin

    part2(in) shouldBe 1
  }

  test("part 2 example 2") {
    val in = """|R 5
                |U 8
                |L 8
                |D 3
                |R 17
                |D 10
                |L 25
                |U 20""".stripMargin

    part2(in) shouldBe 36
  }

  test("part 2") {
    part2(read(file(2022)(9))) shouldBe 2724
  }
