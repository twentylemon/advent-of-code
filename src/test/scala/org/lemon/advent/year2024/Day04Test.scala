package org.lemon.advent.year2024

import org.lemon.advent.*
import org.lemon.advent.year2024.Day04.*

class Day04Test extends UnitTest:

  test("part 1 example") {
    val in = """|MMMSXXMASM
                |MSAMXMSMSA
                |AMXSXMAAMM
                |MSAMASMSMX
                |XMASAMXAMM
                |XXAMMXXAMA
                |SMSMSASXSS
                |SAXAMASAAA
                |MAMMMXMMMM
                |MXMXAXMASX""".stripMargin
    part1(in) shouldBe 18
  }

  test("part 1") {
    part1(read(file(2024)(4))) shouldBe 2514
  }

  test("part 2 example") {
    val in = """|MMMSXXMASM
                |MSAMXMSMSA
                |AMXSXMAAMM
                |MSAMASMSMX
                |XMASAMXAMM
                |XXAMMXXAMA
                |SMSMSASXSS
                |SAXAMASAAA
                |MAMMMXMMMM
                |MXMXAXMASX""".stripMargin
    part2(in) shouldBe 9
  }

  test("part 2") {
    part2(read(file(2024)(4))) shouldBe 1888
  }
