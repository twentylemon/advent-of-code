package org.lemon.advent.year2024

private object Day13:

  type Coord = (Long, Long)

  def parse(input: String) = input.split("\n\n").map(_.linesIterator.toSeq match
    case Seq(
          s"Button A: X+$ax, Y+$ay",
          s"Button B: X+$bx, Y+$by",
          s"Prize: X=$px, Y=$py",
        ) => ((ax.toLong, ay.toLong), (bx.toLong, by.toLong), (px.toLong, py.toLong))
  ).toSeq

  def press(a: Coord, b: Coord, prize: Coord) =
    for
      tryA <- (0 to 100)
      tryB <- (0 to 100)
      if (tryA * a._1 + tryB * b._1, tryA * a._2 + tryB * b._2) == prize
    yield (tryA, tryB)

  def part1(input: String) =
    val machines = parse(input)
    machines
      .map(press.tupled(_).map(3 * _ + _).minOption.getOrElse(0))
      .sum

  def part2(input: String) =
    val machines = parse(input).map((a, b, prize) => (a, b, (prize._1 + 10000000000000L, prize._2 + 10000000000000L)))
    0
