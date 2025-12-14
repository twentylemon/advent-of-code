package org.lemon.advent.year2022

import org.lemon.advent.*
import org.lemon.advent.year2022.Day21.*

class Day21Test extends UnitTest:

  test("part 1 example") {
    val in = """|root: pppw + sjmn
                |dbpl: 5
                |cczh: sllz + lgvd
                |zczc: 2
                |ptdq: humn - dvpt
                |dvpt: 3
                |lfqf: 4
                |humn: 5
                |ljgn: 2
                |sjmn: drzm * dbpl
                |sllz: 4
                |pppw: cczh / lfqf
                |lgvd: ljgn * ptdq
                |drzm: hmdt - zczc
                |hmdt: 32""".stripMargin
    part1(in) shouldBe 152
  }

  test("part 1") {
    part1(read(file(2022)(21))) shouldBe 168502451381566L
  }

  test("part 2 example") {
    val in = """|root: pppw + sjmn
                |dbpl: 5
                |cczh: sllz + lgvd
                |zczc: 2
                |ptdq: humn - dvpt
                |dvpt: 3
                |lfqf: 4
                |humn: 5
                |ljgn: 2
                |sjmn: drzm * dbpl
                |sllz: 4
                |pppw: cczh / lfqf
                |lgvd: ljgn * ptdq
                |drzm: hmdt - zczc
                |hmdt: 32""".stripMargin
    part2(in) shouldBe 301
  }

  test("part 2") {
    part2(read(file(2022)(21))) shouldBe 3343167719435L
  }
