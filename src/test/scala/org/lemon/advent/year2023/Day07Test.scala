package org.lemon.advent.year2023

import org.lemon.advent.*
import org.lemon.advent.year2023.Day07.*

class Day07Test extends UnitTest:

  val in = """|32T3K 765
              |T55J5 684
              |KK677 28
              |KTJJT 220
              |QQQJA 483""".stripMargin

  test("part 1 example") {
    part1(in) shouldBe 6440
  }

  test("part 1") {
    part1(read(file(2023)(7))) shouldBe 249483956
  }

  test("part 2 example") {
    part2(in) shouldBe 5905
  }

  test("part 2") {
    part2(read(file(2023)(7))) shouldBe 252137472
  }
