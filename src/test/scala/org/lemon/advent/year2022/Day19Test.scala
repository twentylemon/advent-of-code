package org.lemon.advent.year2022

import org.lemon.advent.*
import org.lemon.advent.year2022.Day19.*

class Day19Test extends UnitTest:

  test("part 1 blueprint 1") {
    val in =
      """Blueprint 1: Each ore robot costs 4 ore. Each clay robot costs 2 ore. Each obsidian robot costs 3 ore and 14 clay. Each geode robot costs 2 ore and 7 obsidian."""
    optimize(parseBlueprint(in), 24) shouldBe 9
  }

  test("part 1 blueprint 2") {
    val in =
      """Blueprint 2: Each ore robot costs 2 ore. Each clay robot costs 3 ore. Each obsidian robot costs 3 ore and 8 clay. Each geode robot costs 3 ore and 12 obsidian."""
    optimize(parseBlueprint(in), 24) shouldBe 12
  }

  test("part 1 example") {
    val in = """|Blueprint 1: Each ore robot costs 4 ore. Each clay robot costs 2 ore. Each obsidian robot costs 3 ore and 14 clay. Each geode robot costs 2 ore and 7 obsidian.
                |Blueprint 2: Each ore robot costs 2 ore. Each clay robot costs 3 ore. Each obsidian robot costs 3 ore and 8 clay. Each geode robot costs 3 ore and 12 obsidian.""".stripMargin

    part1(in) shouldBe 33
  }

  test("part 1") {
    part1(read(file(2022)(19))) shouldBe 2160
  }

  test("part 2 blueprint 1") {
    val in =
      """Blueprint 1: Each ore robot costs 4 ore. Each clay robot costs 2 ore. Each obsidian robot costs 3 ore and 14 clay. Each geode robot costs 2 ore and 7 obsidian."""
    optimize(parseBlueprint(in), 32) shouldBe 56
  }

  test("part 2 blueprint 2") {
    val in =
      """Blueprint 2: Each ore robot costs 2 ore. Each clay robot costs 3 ore. Each obsidian robot costs 3 ore and 8 clay. Each geode robot costs 3 ore and 12 obsidian."""
    optimize(parseBlueprint(in), 32) shouldBe 62
  }

  test("part 2 example") {
    val in = """|Blueprint 1: Each ore robot costs 4 ore. Each clay robot costs 2 ore. Each obsidian robot costs 3 ore and 14 clay. Each geode robot costs 2 ore and 7 obsidian.
                |Blueprint 2: Each ore robot costs 2 ore. Each clay robot costs 3 ore. Each obsidian robot costs 3 ore and 8 clay. Each geode robot costs 3 ore and 12 obsidian.""".stripMargin

    part2(in) shouldBe 56 * 62
  }

  test("part 2") {
    part2(read(file(2022)(19))) shouldBe 13340
  }
