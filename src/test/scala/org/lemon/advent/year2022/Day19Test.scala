package org.lemon.advent.year2022

import org.lemon.advent._
import scala.collection.mutable
import optimus.optimization._
import optimus.algebra._
import optimus.optimization.enums.SolverLib
import optimus.optimization.model._

@org.scalatest.Ignore // tests are quite slow. python implementation is much faster, like. hours faster
class Day19Test extends UnitTest {

  case class Bank(ore: Int = 0, clay: Int = 0, obsidian: Int = 0, geode: Int = 0)
  case class Robit(cost: Bank)
  case class Blueprint(id: Int, ore: Robit, clay: Robit, obsidian: Robit, geode: Robit)

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
      ore = Robit(cost = Bank(ore = oreOre)),
      clay = Robit(cost = Bank(ore = clayOre)),
      obsidian = Robit(cost = Bank(ore = obsidianOre, clay = obsidianClay)),
      geode = Robit(cost = Bank(ore = geodeOre, obsidian = geodeObsidian))
    )

  def optimize(blueprint: Blueprint, time: Int) =
    given model: MPModel = MPModel(SolverLib.oJSolver)
    val variables = mutable.Map.empty[String, MPVar]

    val earliestOre = blueprint.ore.cost.ore - 1
    val earliestClay = blueprint.clay.cost.ore - 1
    val earliestObsidian = (blueprint.obsidian.cost.ore - 1).max(earliestClay)
    val earliestGeode = (blueprint.geode.cost.ore - 1).max(earliestObsidian)

    for t <- 0 until time do
      // whether we build a robit at time t
      val oreBuild = if t >= earliestOre then MPBinaryVar(s"ore_$t") else MPIntVar(s"ore_$t", 0 to 0)
      val clayBuild = if t >= earliestClay then MPBinaryVar(s"clay_$t") else MPIntVar(s"clay_$t", 0 to 0)
      val obsidianBuild =
        if t >= earliestObsidian then MPBinaryVar(s"obsidian_$t") else MPIntVar(s"obsidian_$t", 0 to 0)
      val geodeBuild = if t >= earliestGeode then MPBinaryVar(s"geode_$t") else MPIntVar(s"geode_$t", 0 to 0)
      subjectTo(oreBuild + clayBuild + obsidianBuild + geodeBuild <:= 1)
      variables ++= Seq(oreBuild, clayBuild, obsidianBuild, geodeBuild).map(v => (v.symbol, v)).toMap

      // keep track of our resource bank
      val banks =
        for u <- 0 until t yield
          val oreFromBuild = (t - u - 1) * variables(s"ore_$u")
          val oreFromOreCost = -blueprint.ore.cost.ore * variables(s"ore_$u")
          val oreFromClayCost = -blueprint.clay.cost.ore * variables(s"clay_$u")
          val oreFromObsidianCost = -blueprint.obsidian.cost.ore * variables(s"obsidian_$u")
          val oreFromGeodeCost = -blueprint.geode.cost.ore * variables(s"geode_$u")
          val oreBank = oreFromBuild + oreFromOreCost + oreFromClayCost + oreFromObsidianCost + oreFromGeodeCost

          val clayFromBuild = (t - u - 1) * variables(s"clay_$u")
          val clayFromObsidianCost = -blueprint.obsidian.cost.clay * variables(s"obsidian_$u")
          val clayBank = clayFromBuild + clayFromObsidianCost

          val obsidianFromBuild = (t - u - 1) * variables(s"obsidian_$u")
          val obsidianFromGeodeCost = -blueprint.geode.cost.obsidian * variables(s"geode_$u")
          val obsidianBank = obsidianFromBuild + obsidianFromGeodeCost

          (oreBank, clayBank, obsidianBank)

      val oreBank = banks.map(_._1).reduceOption(_ + _).getOrElse(Zero) + t
      val clayBank = banks.map(_._2).reduceOption(_ + _).getOrElse(Zero)
      val obsidianBank = banks.map(_._3).reduceOption(_ + _).getOrElse(Zero)

      // only buy robits if we can afford them
      subjectTo(
        blueprint.ore.cost.ore * oreBuild + blueprint.clay.cost.ore * clayBuild + blueprint.obsidian.cost.ore * obsidianBuild + blueprint.geode.cost.ore * geodeBuild <:= oreBank
      )
      subjectTo(blueprint.obsidian.cost.clay * obsidianBuild <:= clayBank)
      subjectTo(blueprint.geode.cost.obsidian * geodeBuild <:= obsidianBank)

    // objective is to maximize the geode bank at the end
    maximize((0 until time).map(t => (time - t - 1) * variables(s"geode_$t")).reduce(_ + _))
    start()
    val max = objectiveValue
    release()
    max.round.toInt

  def part1(in: Seq[String]) = in.map(parseBlueprint).map(blueprint => blueprint.id * optimize(blueprint, 24)).sum

  def part2(in: Seq[String]) = in.take(3).map(parseBlueprint).map(optimize(_, 32)).product

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

    part1(in.linesIterator.toSeq) shouldBe 33
  }

  test("part 1") {
    part1(readLines(file(2022)(19))) shouldBe 2160
  }

  test("part 2 blueprint 1") {
    val in =
      """Blueprint 2: Each ore robot costs 2 ore. Each clay robot costs 3 ore. Each obsidian robot costs 3 ore and 8 clay. Each geode robot costs 3 ore and 12 obsidian."""
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

    part1(in.linesIterator.toSeq) shouldBe 56 * 62
  }

  test("part 2") {
    part1(readLines(file(2022)(19))) shouldBe 13340
  }

}
