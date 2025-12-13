package org.lemon.advent.year2022

private object Day23:

  case class Coord(x: Int, y: Int)
  extension (coord: Coord)
    def N = Coord(coord.x, coord.y - 1)
    def S = Coord(coord.x, coord.y + 1)
    def W = Coord(coord.x - 1, coord.y)
    def E = Coord(coord.x + 1, coord.y)
    def NW = Coord(coord.x - 1, coord.y - 1)
    def NE = Coord(coord.x + 1, coord.y - 1)
    def SW = Coord(coord.x - 1, coord.y + 1)
    def SE = Coord(coord.x + 1, coord.y + 1)

  type Check = (Seq[Coord => Coord], Coord => Coord)
  extension (check: Check)
    def conditions = check._1
    def move = check._2

  def parse(in: Seq[String]): Set[Coord] =
    (for (row, y) <- in.zipWithIndex; (c, x) <- row.zipWithIndex; if c == '#' yield Coord(x, y)).toSet

  def candidate(elf: Coord, round: Int, elves: Set[Coord]) =
    def move(check: Check) =
      if check.conditions.map(f => f(elf)).exists(elves) then None else Some(check.move(elf))

    val shouldChill = (Set(elf.N, elf.S, elf.E, elf.W, elf.NE, elf.NW, elf.SE, elf.SW) & elves).isEmpty
    val checks = Seq((Seq(N, NE, NW), N), (Seq(S, SE, SW), S), (Seq(W, NW, SW), W), (Seq(E, NE, SE), E))
    val check = Iterator.continually(checks).flatMap(x => x).drop(round % checks.size)
    if shouldChill then None
    else move(check.next) orElse move(check.next) orElse move(check.next) orElse move(check.next)

  def simulate(elves: Set[Coord]) =
    Iterator.unfold((elves, 0))((elfyLads, round) =>
      val candidates = elfyLads.map(elf => elf -> candidate(elf, round, elfyLads)).toMap
      val counts = candidates.values.groupBy(x => x).mapValues(_.size)
      val valid = candidates.filter((elf, cand) => cand.isDefined && counts(cand) == 1)
      if valid.isEmpty then None
      else
        val afterMove = elfyLads -- valid.keySet ++ valid.values.flatten.toSet
        Some(afterMove, (afterMove, round + 1))
    )

  def boundingBox(elves: Set[Coord]) =
    (elves.map(_.x).min to elves.map(_.x).max, elves.map(_.y).min to elves.map(_.y).max)

  def part1(in: Seq[String]) =
    val start = parse(in)
    val end = simulate(start).drop(9).next
    val box = boundingBox(end)
    box._1.size * box._2.size - start.size

  def part2(in: Seq[String]) = simulate(parse(in)).size + 1
