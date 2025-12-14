package org.lemon.advent.year2022

import org.lemon.advent.*
import org.lemon.advent.year2022.Day16.*

class Day16Test extends UnitTest:

  test("part 1 example") {
    val in = """|Valve AA has flow rate=0; tunnels lead to valves DD, II, BB
                |Valve BB has flow rate=13; tunnels lead to valves CC, AA
                |Valve CC has flow rate=2; tunnels lead to valves DD, BB
                |Valve DD has flow rate=20; tunnels lead to valves CC, AA, EE
                |Valve EE has flow rate=3; tunnels lead to valves FF, DD
                |Valve FF has flow rate=0; tunnels lead to valves EE, GG
                |Valve GG has flow rate=0; tunnels lead to valves FF, HH
                |Valve HH has flow rate=22; tunnel leads to valve GG
                |Valve II has flow rate=0; tunnels lead to valves AA, JJ
                |Valve JJ has flow rate=21; tunnel leads to valve II""".stripMargin

    part1(in) shouldBe 1651
  }

  test("part 1") {
    part1(read(file(2022)(16))) shouldBe 1737
  }

  test("part 2 example") {
    val in = """|Valve AA has flow rate=0; tunnels lead to valves DD, II, BB
                |Valve BB has flow rate=13; tunnels lead to valves CC, AA
                |Valve CC has flow rate=2; tunnels lead to valves DD, BB
                |Valve DD has flow rate=20; tunnels lead to valves CC, AA, EE
                |Valve EE has flow rate=3; tunnels lead to valves FF, DD
                |Valve FF has flow rate=0; tunnels lead to valves EE, GG
                |Valve GG has flow rate=0; tunnels lead to valves FF, HH
                |Valve HH has flow rate=22; tunnel leads to valve GG
                |Valve II has flow rate=0; tunnels lead to valves AA, JJ
                |Valve JJ has flow rate=21; tunnel leads to valve II""".stripMargin

    part2(in) shouldBe 1707
  }

  test("part 2") {
    part2(read(file(2022)(16))) shouldBe 2216
  }
