package org.lemon.advent.year2022

import org.lemon.advent.lib.*
import org.lemon.advent.lib.ortools.*
import scala.collection.mutable

private object Day19:

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
    val model = MipModel("day19")
    val expr = mutable.Map.empty[String, LinearExpr].withDefaultValue(LinearExpr.Zero)

    for t <- 0 until time do
      // whether we build a robit at time t
      val oreBuild = model.boolVar(s"ore_$t")
      val clayBuild = model.boolVar(s"clay_$t")
      val obsidianBuild = model.boolVar(s"obsidian_$t")
      val geodeBuild = model.boolVar(s"geode_$t")
      model.subjectTo(oreBuild + clayBuild + obsidianBuild + geodeBuild <= 1)
      expr ++= Map(
        s"ore_$t" -> LinearExpr(oreBuild),
        s"clay_$t" -> LinearExpr(clayBuild),
        s"obsidian_$t" -> LinearExpr(obsidianBuild),
        s"geode_$t" -> LinearExpr(geodeBuild)
      )

      val oreCost = blueprint.ore.cost.ore * oreBuild
        + blueprint.clay.cost.ore * clayBuild
        + blueprint.obsidian.cost.ore * obsidianBuild
        + blueprint.geode.cost.ore * geodeBuild
      val clayCost: LinearExpr = blueprint.obsidian.cost.clay * obsidianBuild
      val obsidianCost: LinearExpr = blueprint.geode.cost.obsidian * geodeBuild

      val oreIncome = (0 until t).map(u => expr(s"ore_$u")).reduceOption(_ + _).getOrElse(LinearExpr.Zero) + 1
      val clayIncome = (0 until t).map(u => expr(s"clay_$u")).reduceOption(_ + _).getOrElse(LinearExpr.Zero)
      val obsidianIncome = (0 until t).map(u => expr(s"obsidian_$u")).reduceOption(_ + _).getOrElse(LinearExpr.Zero)
      val geodeIncome = (0 until t).map(u => expr(s"geode_$u")).reduceOption(_ + _).getOrElse(LinearExpr.Zero)

      // we can only build this turn if we had the sources as of the end of last turn
      model.subjectTo(expr(s"oreBank_${t - 1}") - oreCost >= 0)
      model.subjectTo(expr(s"clayBank_${t - 1}") - clayCost >= 0)
      model.subjectTo(expr(s"obsidianBank_${t - 1}") - obsidianCost >= 0)

      val oreBank = expr(s"oreBank_${t - 1}") + oreIncome - oreCost
      val clayBank = expr(s"clayBank_${t - 1}") + clayIncome - clayCost
      val obsidianBank = expr(s"obsidianBank_${t - 1}") + obsidianIncome - obsidianCost
      val geodeBank = expr(s"geodeBank_${t - 1}") + geodeIncome
      expr ++= Map(
        s"oreBank_$t" -> oreBank,
        s"clayBank_$t" -> clayBank,
        s"obsidianBank_$t" -> obsidianBank,
        s"geodeBank_$t" -> geodeBank
      )

    // we can only build one robot per turn, don't bother to have more income than that
    val maxOre =
      blueprint.ore.cost.ore.max(blueprint.clay.cost.ore).max(blueprint.obsidian.cost.ore).max(blueprint.geode.cost.ore)
    model.subjectTo((0 until time).map(t => expr(s"ore_$t")).reduce(_ + _) <= maxOre)
    model.subjectTo((0 until time).map(t => expr(s"clay_$t")).reduce(_ + _) <= blueprint.obsidian.cost.clay)
    model.subjectTo((0 until time).map(t => expr(s"obsidian_$t")).reduce(_ + _) <= blueprint.geode.cost.obsidian)

    // don't bother building robots if they can't contribute to geodes later
    model.subjectTo(((time - 3) until time).map(t => expr(s"ore_$t")).reduce(_ + _) === 0)
    model.subjectTo(((time - 3) until time).map(t => expr(s"clay_$t")).reduce(_ + _) === 0)
    model.subjectTo(((time - 2) until time).map(t => expr(s"obsidian_$t")).reduce(_ + _) === 0)
    model.subjectTo(((time - 1) until time).map(t => expr(s"geode_$t")).reduce(_ + _) === 0)

    // objective is to maximize the geode bank at the end
    model.maximize(expr(s"geodeBank_${time - 1}"))
    model.solve()
    val max = model.objective
    model.release()
    max.round.toInt

  def part1(in: Seq[String]) = in.map(parseBlueprint).map(blueprint => blueprint.id * optimize(blueprint, 24)).sum

  def part2(in: Seq[String]) = in.take(3).map(parseBlueprint).map(optimize(_, 32)).product
