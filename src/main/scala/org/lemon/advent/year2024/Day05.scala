package org.lemon.advent.year2024

import org.lemon.advent.lib.*
import org.lemon.advent.lib.graph.*

private object Day05:

  case class Rule(before: Int, after: Int)

  def parse(input: String) =
    val Seq(rules, pages) = input.chunks
    val r = rules.linesIterator.map { case s"$before|$after" => Rule(before.toInt, after.toInt) }.toSeq
    val p = pages.linesIterator.map(_.csv.map(_.toInt)).toSeq
    (r, p)

  /** Topologically sorts the pages according to the rules that apply to them. */
  def order(pages: Seq[Int], rules: Seq[Rule]): Seq[Int] =
    val pageSet = pages.toSet
    val adjacency = rules
      .filter(r => pageSet.contains(r.before) && pageSet.contains(r.after))
      .groupMap(_.before)(_.after)
      .withDefaultValue(Seq.empty)
    topologicalSort(pages, adjacency).get

  def part1(input: String) =
    val (rules, pages) = parse(input)
    pages.filter(p => order(p, rules) == p).map(p => p(p.size / 2)).sum

  def part2(input: String) =
    val (rules, pages) = parse(input)
    pages.flatMap { p =>
      val sorted = order(p, rules)
      Option.when(sorted != p)(sorted(sorted.size / 2))
    }.sum
