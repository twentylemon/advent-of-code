package org.lemon.advent.year2025

import org.lemon.advent.*
import org.lemon.advent.year2025.Day02.*

class Day02Test extends UnitTest:

  test("part 1 example") {
    val in = """|11-22,95-115,998-1012,1188511880-1188511890,222220-222224,
                |1698522-1698528,446443-446449,38593856-38593862,565653-565659,
                |824824821-824824827,2121212118-2121212124
                |""".stripMargin
    part1(in) shouldBe 1227775554L
  }

  test("part 1") {
    part1(read(file(2025)(2))) shouldBe 24043483400L
  }

  test("part 2 example") {
    val in = """|11-22,95-115,998-1012,1188511880-1188511890,222220-222224,
                |1698522-1698528,446443-446449,38593856-38593862,565653-565659,
                |824824821-824824827,2121212118-2121212124
                |""".stripMargin
    part2(in) shouldBe 4174379265L
  }

  test("part 2") {
    part2(read(file(2025)(2))) shouldBe 38262920235L
  }
