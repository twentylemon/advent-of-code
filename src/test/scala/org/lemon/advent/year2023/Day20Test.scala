package org.lemon.advent.year2023

import org.lemon.advent._
import org.lemon.advent.year2023.Day20._

class Day20Test extends UnitTest:

  test("part 1 example small") {
    val in = """|broadcaster -> a, b, c
                |%a -> b
                |%b -> c
                |%c -> inv
                |&inv -> a
                |""".stripMargin
    part1(in) shouldBe 32000000L
  }

  test("part 1 example big") {
    val in = """|broadcaster -> a
                |%a -> inv, con
                |&inv -> b
                |%b -> con
                |&con -> output
                |""".stripMargin
    part1(in) shouldBe 11687500L
  }

  test("part 1") {
    part1(read(file(2023)(20))) shouldBe 666795063L
  }

  test("part 2") {
    part2(read(file(2023)(20))) shouldBe 253302889093151L
  }
