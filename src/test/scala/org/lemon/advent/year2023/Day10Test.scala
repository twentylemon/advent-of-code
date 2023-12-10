package org.lemon.advent.year2023

import org.lemon.advent._
import org.lemon.advent.year2023.Day10._

class Day10Test extends UnitTest:

  test("part 1 example square quiet") {
    val in = """|.....
                |.S-7.
                |.|.|.
                |.L-J.
                |.....""".stripMargin
    part1(in) shouldBe 4
  }

  test("part 1 example square noisy") {
    val in = """|-L|F7
                |7S-7|
                |L|7||
                |-L-J|
                |L|-JF""".stripMargin
    part1(in) shouldBe 4
  }

  test("part 1 example complex quiet") {
    val in = """|..F7.
                |.FJ|.
                |SJ.L7
                |F--J
                |LJ...""".stripMargin
    part1(in) shouldBe 8
  }

  test("part 1 example complex noisy") {
    val in = """|7-F7-
                |.FJ|7
                |SJLL7
                |F--J
                |LJ.LJF""".stripMargin
    part1(in) shouldBe 8
  }

  test("part 1") {
    part1(read(file(2023)(10))) shouldBe 6846
  }

  test("part 2 example open") {
    val in = """|...........
                |.S-------7.
                |.|F-----7|.
                |.||.....||.
                |.||.....||.
                |.|L-7.F-J|.
                |.|..|.|..|.
                |.L--J.L--J.
                |...........""".stripMargin
    part2(in) shouldBe 4
  }

  test("part 2 example closed") {
    val in = """|..........
                |.S------7.
                |.|F----7|.
                |.||OOOO||.
                |.||OOOO||.
                |.|L-7F-J|.
                |.|II||II|.
                |.L--JL--J.
                |..........""".stripMargin
    part2(in) shouldBe 4
  }

  test("part 2 example large 1") {
    val in = """|OF----7F7F7F7F-7OOOO
                |O|F--7||||||||FJOOOO
                |O||OFJ||||||||L7OOOO
                |FJL7L7LJLJ||LJIL-7OO
                |L--JOL7IIILJS7F-7L7O
                |OOOOF-JIIF7FJ|L7L7L7
                |OOOOL7IF7||L7|IL7L7|
                |OOOOO|FJLJ|FJ|F7|OLJ
                |OOOOFJL-7O||O||||OOO
                |OOOOL---JOLJOLJLJOOO""".stripMargin
    part2(in) shouldBe 8
  }

  test("part 2 example large 2") {
    val in = """|FF7FSF7F7F7F7F7F---7
                |L|LJ||||||||||||F--J
                |FL-7LJLJ||||||LJL-77
                |F--JF--7||LJLJIF7FJ-
                |L---JF-JLJIIIIFJLJJ7
                ||F|F-JF---7IIIL7L|7|
                ||FFJF7L7F-JF7IIL---7
                |7-L-JL7||F7|L7F-7F7|
                |L.L7LFJ|||||FJL7||LJ
                |L7JLJL-JLJLJL--JLJ.L""".stripMargin

    part2(in) shouldBe 10
  }

  test("part 2") {
    part2(read(file(2023)(10))) shouldBe 325
  }
