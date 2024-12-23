package org.lemon.advent.year2024

import org.lemon.advent._
import org.lemon.advent.year2024.Day23._

class Day23Test extends UnitTest:

  test("part 1 example") {
    val in = """|kh-tc
                |qp-kh
                |de-cg
                |ka-co
                |yn-aq
                |qp-ub
                |cg-tb
                |vc-aq
                |tb-ka
                |wh-tc
                |yn-cg
                |kh-ub
                |ta-co
                |de-co
                |tc-td
                |tb-wq
                |wh-td
                |ta-ka
                |td-qp
                |aq-cg
                |wq-ub
                |ub-vc
                |de-ta
                |wq-aq
                |wq-vc
                |wh-yn
                |ka-de
                |kh-ta
                |co-tc
                |wh-qp
                |tb-vc
                |td-yn""".stripMargin
    part1(in) shouldBe 7
  }

  test("part 1") {
    part1(read(file(2024)(23))) shouldBe 1269
  }

  test("part 2 example") {
    val in = """|kh-tc
                |qp-kh
                |de-cg
                |ka-co
                |yn-aq
                |qp-ub
                |cg-tb
                |vc-aq
                |tb-ka
                |wh-tc
                |yn-cg
                |kh-ub
                |ta-co
                |de-co
                |tc-td
                |tb-wq
                |wh-td
                |ta-ka
                |td-qp
                |aq-cg
                |wq-ub
                |ub-vc
                |de-ta
                |wq-aq
                |wq-vc
                |wh-yn
                |ka-de
                |kh-ta
                |co-tc
                |wh-qp
                |tb-vc
                |td-yn""".stripMargin
    part2(in) shouldBe "co,de,ka,ta"
  }

  test("part 2") {
    part2(read(file(2024)(23))) shouldBe "ad,jw,kt,kz,mt,nc,nr,sb,so,tg,vs,wh,yh"
  }
