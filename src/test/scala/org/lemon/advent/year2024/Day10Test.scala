package org.lemon.advent.year2024

import org.lemon.advent._
import org.lemon.advent.year2024.Day10._

class Day10Test extends UnitTest:

  test("part 1 example") {
    val in = """|89010123
                |78121874
                |87430965
                |96549874
                |45678903
                |32019012
                |01329801
                |10456732""".stripMargin
    part1(in) shouldBe 36
  }

  test("part 1") {
    part1(read(file(2024)(10))) shouldBe 548
  }

  test("part 2 example") {
    val in = """|89010123
                |78121874
                |87430965
                |96549874
                |45678903
                |32019012
                |01329801
                |10456732""".stripMargin
    part2(in) shouldBe 81
  }

  test("part 2") {
    part2(read(file(2024)(10))) shouldBe 1252
  }
