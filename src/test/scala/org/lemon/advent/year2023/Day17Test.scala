package org.lemon.advent.year2023

import org.lemon.advent._
import org.lemon.advent.year2023.Day17._

class Day17Test extends UnitTest:

  val in = """|2413432311323
              |3215453535623
              |3255245654254
              |3446585845452
              |4546657867536
              |1438598798454
              |4457876987766
              |3637877979653
              |4654967986887
              |4564679986453
              |1224686865563
              |2546548887735
              |4322674655533""".stripMargin

  test("part 1 example") {
    part1(in) shouldBe 102
  }

  test("part 1") {
    part1(read(file(2023)(17))) shouldBe 967
  }

  test("part 2 example 1") {
    part2(in) shouldBe 94
  }

  test("part 2 example 2") {
    val in = """|111111111111
                |999999999991
                |999999999991
                |999999999991
                |999999999991
                |999999999991
                |999999999991
                |999999999991
                |999999999991
                |999999999991
                |999999999991
                |999999999991""".stripMargin
    part2(in) shouldBe 78
  }

  test("part 2") {
    part2(read(file(2023)(17))) shouldBe 1101
  }
