package org.lemon.advent.year2022

import org.lemon.advent._
import scala.collection.mutable

/** This was written after the fact, after seeing others get good runtimes in scala.
  * @see
  *   [[Day19Test]] for the original linear program, or the python implementation or what I actually used
  */
class Day19PosthumousTest extends UnitTest {

  case class Resources(ore: Int = 0, clay: Int = 0, obsidian: Int = 0, geode: Int = 0):
    def +(rhs: Resources) =
      copy(ore = ore + rhs.ore, clay = clay + rhs.clay, obsidian = obsidian + rhs.obsidian, geode = geode + rhs.geode)

    def -(rhs: Resources) =
      copy(ore = ore - rhs.ore, clay = clay - rhs.clay, obsidian = obsidian - rhs.obsidian, geode = geode - rhs.geode)

    def *(rhs: Int) = copy(ore = ore * rhs, clay = clay * rhs, obsidian = obsidian * rhs, geode = geode * rhs)

  case class Blueprint(id: Int, ore: Resources, clay: Resources, obsidian: Resources, geode: Resources):
    val robots = Seq(ore, clay, obsidian, geode)
    val maxOreRobots = robots.map(_.ore).max
    val maxClayRobots = robots.map(_.clay).max
    val maxObsidianRobots = robots.map(_.obsidian).max

  def parseBlueprint(in: String) =
    val id = in match { case s"Blueprint $x: $_" => x.toInt }
    val oreOre = in match { case s"$_ Each ore robot costs $n ore. $_" => n.toInt }
    val clayOre = in match { case s"$_ Each clay robot costs $n ore. $_" => n.toInt }
    val (obsidianOre, obsidianClay) =
      in match { case s"$_ Each obsidian robot costs $n ore and $m clay. $_" => (n.toInt, m.toInt) }
    val (geodeOre, geodeObsidian) =
      in match { case s"$_ Each geode robot costs $n ore and $m obsidian." => (n.toInt, m.toInt) }
    Blueprint(
      id = id,
      ore = Resources(ore = oreOre),
      clay = Resources(ore = clayOre),
      obsidian = Resources(ore = obsidianOre, clay = obsidianClay),
      geode = Resources(ore = geodeOre, obsidian = geodeObsidian)
    )

  case class State(robots: Resources, bank: Resources, time: Int):
    def addRobot(amount: Resources, cost: Resources) = copy(robots = robots + amount, bank = bank - cost)

    def canAfford(cost: Resources) =
      cost.ore <= bank.ore && cost.clay <= bank.clay && cost.obsidian <= bank.obsidian && cost.geode <= bank.geode

    def tick(time: Int) = copy(time = this.time - time, bank = bank + robots * time)

    def tickUntil(predicate: State => Boolean, minTime: Int) = Iterator.iterate(this)(_.tick(1)).dropWhile(s => s.time >= minTime && !predicate(s)).nextOption

  def isBetterThan(lhs: State, rhs: State) =
    lhs.bank.geode > rhs.bank.geode && lhs.robots.geode > rhs.robots.geode && lhs.robots.obsidian > rhs.robots.obsidian

  def solve(blueprint: Blueprint, time: Int): Int =
    val initialState = State(robots=Resources(ore=1), bank=Resources(), time=time)
    val best = Map.empty[Int, State].withDefaultValue(initialState)
    val queue = mutable.PriorityQueue(initialState)(Ordering.by(state => (state.time, state.bank.geode)))

    def iterate(queue: mutable.PriorityQueue[State], best: Map[Int, State]): Int =
      val state = queue.dequeue
      if state.time <= 0 then state.bank.geode
      else if isBetterThan(best(state.time), state) then iterate(queue, best)
      else
        val canBuildGeode = state.canAfford(blueprint.geode)

        val buildOre = Option.when(state.robots.ore < blueprint.maxOreRobots && !canBuildGeode)(
          state.tickUntil(_.canAfford(blueprint.ore), 3)
        ).flatten.map(_.tick(1).addRobot(Resources(ore = 1), blueprint.ore))

        val buildClay = Option.when(state.robots.clay < blueprint.maxClayRobots && !canBuildGeode)(
          state.tickUntil(_.canAfford(blueprint.clay), 3)
        ).flatten.map(_.tick(1).addRobot(Resources(clay = 1), blueprint.clay))

        val buildObsidian = Option.when(state.robots.clay > 0 && state.robots.obsidian < blueprint.maxObsidianRobots && !canBuildGeode)(
          state.tickUntil(_.canAfford(blueprint.obsidian), 3)
        ).flatten.map(_.tick(1).addRobot(Resources(obsidian = 1), blueprint.obsidian))

        val buildGeode = Option.when(state.robots.obsidian > 0)(
          state.tickUntil(_.canAfford(blueprint.geode), 2)
        ).flatten.map(_.tick(1).addRobot(Resources(geode = 1), blueprint.geode))

        val branches = Seq(buildGeode, buildObsidian, buildClay, buildOre).flatten
        queue.enqueue(branches: _*)
        iterate(queue, best + (state.time -> state))
    
    iterate(queue, best)


  def part1(in: Seq[String]) = in.map(parseBlueprint).map(blueprint => blueprint.id * solve(blueprint, 24)).sum

  def part2(in: Seq[String]) = in.take(3).map(parseBlueprint).map(solve(_, 32)).product

  test("part 1 blueprint 1") {
    val in =
      """Blueprint 1: Each ore robot costs 4 ore. Each clay robot costs 2 ore. Each obsidian robot costs 3 ore and 14 clay. Each geode robot costs 2 ore and 7 obsidian."""
    solve(parseBlueprint(in), 24) shouldBe 9
  }

  test("part 1 blueprint 2") {
    val in =
      """Blueprint 2: Each ore robot costs 2 ore. Each clay robot costs 3 ore. Each obsidian robot costs 3 ore and 8 clay. Each geode robot costs 3 ore and 12 obsidian."""
    solve(parseBlueprint(in), 24) shouldBe 12
  }

  test("part 1 example") {
    val in = """|Blueprint 1: Each ore robot costs 4 ore. Each clay robot costs 2 ore. Each obsidian robot costs 3 ore and 14 clay. Each geode robot costs 2 ore and 7 obsidian.
                |Blueprint 2: Each ore robot costs 2 ore. Each clay robot costs 3 ore. Each obsidian robot costs 3 ore and 8 clay. Each geode robot costs 3 ore and 12 obsidian.""".stripMargin

    part1(in.linesIterator.toSeq) shouldBe 33
  }

  test("part 1") {
    part1(readLines(file(2022)(19))) shouldBe 2160
  }

  // is quite a slow boy -- slower than the real deal. /shrug
  ignore("part 2 example") {
    val in = """|Blueprint 1: Each ore robot costs 4 ore. Each clay robot costs 2 ore. Each obsidian robot costs 3 ore and 14 clay. Each geode robot costs 2 ore and 7 obsidian.
                |Blueprint 2: Each ore robot costs 2 ore. Each clay robot costs 3 ore. Each obsidian robot costs 3 ore and 8 clay. Each geode robot costs 3 ore and 12 obsidian.""".stripMargin

    part2(in.linesIterator.toSeq) shouldBe 56 * 62
  }

  test("part 2") {
    part2(readLines(file(2022)(19))) shouldBe 13340
  }

}
