package org.lemon.advent.year2016

import org.lemon.advent.*
import org.lemon.advent.year2016.Day07.*

class Day07Test extends UnitTest:

  test("part 1 example") {
    val in = """|abba[mnop]qrst
                |abcd[bddb]xyyx
                |aaaa[qwer]tyui
                |ioxxoj[asdfgh]zxcvbn
                |""".stripMargin
    part1(in) shouldBe 2
  }

  test("part 1") {
    part1(read(file(2016)(7))) shouldBe 110
  }

  test("part 2 example") {
    val in = """|aba[bab]xyz
                |xyx[xyx]xyx
                |aaa[kek]eke
                |zazbz[bzb]cdb
                |""".stripMargin
    part2(in) shouldBe 3
  }

  test("part 2") {
    part2(read(file(2016)(7))) shouldBe 242
  }
