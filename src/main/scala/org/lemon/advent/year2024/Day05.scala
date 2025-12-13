package org.lemon.advent.year2024

import org.lemon.advent.lib.*

private object Day05:

  case class Rule(before: Int, after: Int)

  def parse(input: String) =
    val Seq(rules, pages) = input.chunks
    val r = rules.linesIterator.map { case s"$before|$after" => Rule(before.toInt, after.toInt) }.toSeq
    val p = pages.linesIterator.map(_.csv.map(_.toInt)).toSeq
    (r, p)

  @annotation.tailrec
  def isOrdered(pages: Seq[Int], rules: Seq[Rule]): Boolean =
    if rules.isEmpty then true
    else
      val rule = rules.head
      val before = pages.indexOf(rule.before)
      val after = pages.indexOf(rule.after)
      if before < 0 || after < 0 || before < after then isOrdered(pages, rules.tail)
      else false

  def part1(input: String) =
    val (rules, pages) = parse(input)
    pages.filter(isOrdered(_, rules)).map(p => p(p.size / 2)).sum

  def fixOrder(pages: Seq[Int], rules: Seq[Rule]): Seq[Int] =
    pages.sortWith((a, b) => rules.exists(r => r.before == a && r.after == b))

  def part2(input: String) =
    val (rules, pages) = parse(input)
    pages.filterNot(isOrdered(_, rules)).map(fixOrder(_, rules)).map(p => p(p.size / 2)).sum
