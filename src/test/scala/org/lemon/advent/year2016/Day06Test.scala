package org.lemon.advent.year2016

import org.lemon.advent.*
import org.lemon.advent.year2016.Day06.*

class Day06Test extends UnitTest:

  test("part 1 example") {
    val in = """|eedadn
                |drvtee
                |eandsr
                |raavrd
                |atevrs
                |tsrnev
                |sdttsa
                |rasrtv
                |nssdts
                |ntnada
                |svetve
                |tesnvt
                |vntsnd
                |vrdear
                |dvrsen
                |enarar
                |""".stripMargin
    part1(in) shouldBe "easter"
  }

  test("part 1") {
    part1(read(file(2016)(6))) shouldBe "qqqluigu"
  }

  test("part 2 example") {
    val in = """|eedadn
                |drvtee
                |eandsr
                |raavrd
                |atevrs
                |tsrnev
                |sdttsa
                |rasrtv
                |nssdts
                |ntnada
                |svetve
                |tesnvt
                |vntsnd
                |vrdear
                |dvrsen
                |enarar
                |""".stripMargin
    part2(in) shouldBe "advent"
  }

  test("part 2") {
    part2(read(file(2016)(6))) shouldBe "lsoypmia"
  }
