package org.lemon.advent.year2025

import org.lemon.advent.*
import org.lemon.advent.year2025.Day10.*

class Day10Test extends UnitTest:

  test("part 1 example 1") {
    val in = "[.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}"
    part1(in) shouldBe 2
  }

  test("part 1 example 2") {
    val in = "[...#.] (0,2,3,4) (2,3) (0,4) (0,1,2) (1,2,3,4) {7,5,12,7,2}"
    part1(in) shouldBe 3
  }

  test("part 1 example 3") {
    val in = "[.###.#] (0,1,2,3,4) (0,3,4) (0,1,2,4,5) (1,2) {10,11,11,5,10,5}"
    part1(in) shouldBe 2
  }

  test("part 1 example") {
    val in = """|[.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}
                |[...#.] (0,2,3,4) (2,3) (0,4) (0,1,2) (1,2,3,4) {7,5,12,7,2}
                |[.###.#] (0,1,2,3,4) (0,3,4) (0,1,2,4,5) (1,2) {10,11,11,5,10,5}
                |""".stripMargin
    part1(in) shouldBe 7
  }

  test("part 1") {
    part1(read(file(2025)(10))) shouldBe 399
  }

  test("part 2 example 1") {
    val in = "[.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}"
    part2(in) shouldBe 10
  }

  test("part 2 example 2") {
    val in = "[...#.] (0,2,3,4) (2,3) (0,4) (0,1,2) (1,2,3,4) {7,5,12,7,2}"
    part2(in) shouldBe 12
  }

  test("part 2 example 3") {
    val in = "[.###.#] (0,1,2,3,4) (0,3,4) (0,1,2,4,5) (1,2) {10,11,11,5,10,5}"
    part2(in) shouldBe 11
  }

  test("part 2 example") {
    val in = """|[.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}
                |[...#.] (0,2,3,4) (2,3) (0,4) (0,1,2) (1,2,3,4) {7,5,12,7,2}
                |[.###.#] (0,1,2,3,4) (0,3,4) (0,1,2,4,5) (1,2) {10,11,11,5,10,5}
                |""".stripMargin
    part2(in) shouldBe 33
  }

  test("part 2") {
    part2(read(file(2025)(10))) shouldBe 15631
  }
