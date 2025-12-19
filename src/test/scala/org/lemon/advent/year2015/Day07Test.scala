package org.lemon.advent.year2015

import org.lemon.advent.*
import org.lemon.advent.year2015.Day07.*

class Day07Test extends UnitTest:

  for (wire, expected) <- Seq(
    "d" -> 72,
    "e" -> 507,
    "f" -> 492,
    "g" -> 114,
    "h" -> 65412,
    "i" -> 65079,
    "x" -> 123,
    "y" -> 456,
  ) do 
  test(s"part 1 example $wire") {
    val in = """|123 -> x
                |456 -> y
                |x AND y -> d
                |x OR y -> e
                |x LSHIFT 2 -> f
                |y RSHIFT 2 -> g
                |NOT x -> h
                |NOT y -> i
                |""".stripMargin
    part1(in, output = wire) shouldBe expected
  }

  test("part 1") {
    part1(read(file(2015)(7))) shouldBe 956
  }

  test("part 2") {
    part2(read(file(2015)(7))) shouldBe 40149
  }
