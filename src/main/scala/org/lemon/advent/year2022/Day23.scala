package org.lemon.advent.year2022

import org.lemon.advent.lib.*
import org.lemon.advent.lib.`2d`.*

private object Day23:

  type Check = (Seq[Coord => Coord], Coord => Coord)
  extension (check: Check)
    def conditions = check._1
    def move = check._2

  def u(c: Coord) = c.up
  def d(c: Coord) = c.down
  def l(c: Coord) = c.left
  def r(c: Coord) = c.right
  def ul(c: Coord) = c.up.left
  def ur(c: Coord) = c.up.right
  def dl(c: Coord) = c.down.left
  def dr(c: Coord) = c.down.right

  def parse(in: String): Set[Coord] =
    Coord.gridToMap(in).filter(_._2 == '#').keySet

  def candidate(elf: Coord, round: Int, elves: Set[Coord]) =
    def move(check: Check) =
      if check.conditions.map(f => f(elf)).exists(elves) then None else Some(check.move(elf))

    val shouldChill = (elf.surrounding.toSet & elves).isEmpty
    val checks = Seq((Seq(u, ur, ul), u), (Seq(d, dr, dl), d), (Seq(l, ul, dl), l), (Seq(r, ur, dr), r))
    val check = Iterator.continually(checks).flatMap(x => x).drop(round % checks.size)
    if shouldChill then None
    else move(check.next) orElse move(check.next) orElse move(check.next) orElse move(check.next)

  def simulate(elves: Set[Coord]) =
    Iterator.unfold((elves, 0))((elfyLads, round) =>
      val candidates = elfyLads.map(elf => elf -> candidate(elf, round, elfyLads)).toMap
      val counts = candidates.values.frequencies
      val valid = candidates.filter((elf, cand) => cand.isDefined && counts(cand) == 1)
      if valid.isEmpty then None
      else
        val afterMove = elfyLads -- valid.keySet ++ valid.values.flatten.toSet
        Some(afterMove, (afterMove, round + 1))
    )

  def part1(in: String) =
    val start = parse(in)
    val end = simulate(start).nth(9)
    Area(end).size - start.size

  def part2(in: String) = simulate(parse(in)).size + 1
