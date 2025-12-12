package org.lemon.advent.year2022

import org.lemon.advent.*
import org.lemon.advent.year2022.Day11.*

class Day11Test extends UnitTest:

  test("part 1 example") {
    val in = """|Monkey 0:
                |  Starting items: 79, 98
                |  Operation: new = old * 19
                |  Test: divisible by 23
                |    If true: throw to monkey 2
                |    If false: throw to monkey 3
                |
                |Monkey 1:
                |  Starting items: 54, 65, 75, 74
                |  Operation: new = old + 6
                |  Test: divisible by 19
                |    If true: throw to monkey 2
                |    If false: throw to monkey 0
                |
                |Monkey 2:
                |  Starting items: 79, 60, 97
                |  Operation: new = old * old
                |  Test: divisible by 13
                |    If true: throw to monkey 1
                |    If false: throw to monkey 3
                |
                |Monkey 3:
                |  Starting items: 74
                |  Operation: new = old + 3
                |  Test: divisible by 17
                |    If true: throw to monkey 0
                |    If false: throw to monkey 1""".stripMargin

    part1(in) shouldBe 10605
  }

  test("part 1") {
    part1(read(file(2022)(11))) shouldBe 100345
  }

  test("part 2 example") {
    val in = """|Monkey 0:
                |  Starting items: 79, 98
                |  Operation: new = old * 19
                |  Test: divisible by 23
                |    If true: throw to monkey 2
                |    If false: throw to monkey 3
                |
                |Monkey 1:
                |  Starting items: 54, 65, 75, 74
                |  Operation: new = old + 6
                |  Test: divisible by 19
                |    If true: throw to monkey 2
                |    If false: throw to monkey 0
                |
                |Monkey 2:
                |  Starting items: 79, 60, 97
                |  Operation: new = old * old
                |  Test: divisible by 13
                |    If true: throw to monkey 1
                |    If false: throw to monkey 3
                |
                |Monkey 3:
                |  Starting items: 74
                |  Operation: new = old + 3
                |  Test: divisible by 17
                |    If true: throw to monkey 0
                |    If false: throw to monkey 1""".stripMargin

    part2(in) shouldBe 2713310158L
  }

  test("part 2") {
    part2(read(file(2022)(11))) shouldBe 28537348205L
  }
