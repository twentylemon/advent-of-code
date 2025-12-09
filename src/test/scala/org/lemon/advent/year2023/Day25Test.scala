package org.lemon.advent.year2023

import org.lemon.advent.*
import org.lemon.advent.year2023.Day25.*

class Day25Test extends UnitTest:

  val in = """|jqt: rhn xhk nvd
              |rsh: frs pzl lsr
              |xhk: hfx
              |cmg: qnr nvd lhk bvb
              |rhn: xhk bvb hfx
              |bvb: xhk hfx
              |pzl: lsr hfx nvd
              |qnr: nvd
              |ntq: jqt hfx bvb xhk
              |nvd: lhk
              |lsr: lhk
              |rzs: qnr cmg lsr rsh
              |frs: qnr lhk lsr
              |""".stripMargin

  test("part 1 example") {
    part1(in) shouldBe 54
  }

  test("part 1") {
    part1(read(file(2023)(25))) shouldBe 548960
  }
